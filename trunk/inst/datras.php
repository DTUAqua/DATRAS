<?php
/**
 datras.php
 
 Script for downloading DATRAS data.
 
 Requires the PHP command line interface installed (php5-cli in Debian).
 
 Authors: Kasper Kristensen and Casper W. Berg, DTU Aqua.
*/

if(count($argv)<2){
  print("\n");
  print("Script for downloading DATRAS data from www.datras.ices.dk\n");
  print("Usage: php datras.php survey [year]\n");
  print("\n");
  print("Example: php datras.php NS-IBTS 2008 \n");
  print("If [year] is omitted, then all years are downloaded\n");
  print("\n");
  print("Available surveys:\n");

  $url = "http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx";
  $ckfile = tempnam("/tmp", "CURLCOOKIE");
  $useragent = 'Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/533.2 (KHTML, like Gecko) Chrome/5.0.342.3 Safari/533.2';
  
  $username = "XXXXXXXXXX";
  $password = "XXXXXXXXXX";

  $ch = curl_init($url);
  curl_setopt($ch, CURLOPT_COOKIEJAR, $ckfile);
  curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($ch, CURLOPT_USERAGENT, $useragent);
  
  $html = curl_exec($ch);
  
  curl_close($ch);

  preg_match_all('~<option.*value=".*">(.*?)</option>~', $html, $surveys);
  for($i=11;$i<count($surveys[1]); $i++)
    print($surveys[1][$i] . "\n");
  die();
}

function updatecodes($postfields,$ret){
  $viewstate=array();
  $eventValidation=array();
  preg_match('~<input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="(.*?)" />~', $ret, $viewstate);
  preg_match('~<input type="hidden" name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="(.*?)" />~', $ret, $eventValidation);
  $viewstate = $viewstate[1];
  $eventValidation = $eventValidation[1];
  $postfields['__VIEWSTATE'] = $viewstate;
  $postfields['__EVENTVALIDATION'] = $eventValidation;
  return $postfields;
};


$url = "http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx";
$ckfile = tempnam("/tmp", "CURLCOOKIE");
$useragent = 'Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/533.2 (KHTML, like Gecko) Chrome/5.0.342.3 Safari/533.2';

$username = "XXXXXXXXXX";
$password = "XXXXXXXXXX";


$f = fopen('log.txt', 'w'); // file to write request header for debug purpose

/**
   Get __VIEWSTATE & __EVENTVALIDATION
*/
$ch = curl_init($url);
curl_setopt($ch, CURLOPT_COOKIEJAR, $ckfile);
curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_USERAGENT, $useragent);

$html = curl_exec($ch);

curl_close($ch);

preg_match('~<input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="(.*?)" />~', $html, $viewstate);
preg_match('~<input type="hidden" name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="(.*?)" />~', $html, $eventValidation);
$viewstate = $viewstate[1];
$eventValidation = $eventValidation[1];

//

// <option value="2341">NS-IBTS</option>
$survey=$argv[1];
preg_match('~<option.*value="(.*?)">[ ]*' . $survey . '[ ]*</option>~', $html, $surveyid);
//print_r($surveyid);
if(count($surveyid)!=2)die("Non-unique survey");
$surveyid=$surveyid[1];

/**
   Start Login process
*/
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_COOKIEJAR, $ckfile);
curl_setopt($ch, CURLOPT_COOKIEFILE, $ckfile);
curl_setopt($ch, CURLOPT_HEADER, FALSE);
curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
curl_setopt($ch, CURLOPT_REFERER, $url);
curl_setopt($ch, CURLOPT_VERBOSE, 1);
curl_setopt($ch, CURLOPT_STDERR, $f);
curl_setopt($ch, CURLOPT_USERAGENT, $useragent);

// Collecting all POST fields
$postfields = array();
$postfields['__EVENTTARGET'] = "";
$postfields['__EVENTARGUMENT'] = "";
$postfields['__VIEWSTATE'] = $viewstate;
$postfields['__EVENTVALIDATION'] = $eventValidation;
$postfields['ctl00$ContentPlaceHolder1$cblist_quarter$0'] = 'All';
$postfields['ctl00$ContentPlaceHolder1$ChkHH'] = "checked";
// Set survey
$postfields['ctl00$ContentPlaceHolder1$ddl_survey'] = "$surveyid";

curl_setopt($ch, CURLOPT_POST, 1);
curl_setopt($ch, CURLOPT_POSTFIELDS, $postfields);
$ret = curl_exec($ch); // Get result after login page.


//<input id="ContentPlaceHolder1_cblist_years"
$ret=str_replace("<input","\n<input",$ret);
//print $ret;
//die();
preg_match_all('~<input id="ContentPlaceHolder1_cblist_years.*value="(.*?)"~', $ret, $years);
/* print_r($years); */
/* die(); */
$years=array_slice($years[1],1);

/* print_r($years); */
/* die(); */

$allyears=$years;
if(isset($argv[2])) $years=array($argv[2]);

$origstate=$postfields;


foreach($years as $year){
  
  //Init
  $ch = curl_init();

  curl_setopt($ch, CURLOPT_URL, $url);
  curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($ch, CURLOPT_COOKIEJAR, $ckfile);
  curl_setopt($ch, CURLOPT_COOKIEFILE, $ckfile);
  curl_setopt($ch, CURLOPT_HEADER, FALSE);
  curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
  curl_setopt($ch, CURLOPT_REFERER, $url);
  curl_setopt($ch, CURLOPT_VERBOSE, 1);
  curl_setopt($ch, CURLOPT_STDERR, $f);
  curl_setopt($ch, CURLOPT_USERAGENT, $useragent);

  // Collecting all POST fields
  $postfields = array();
  $postfields['__EVENTTARGET'] = "";
  $postfields['__EVENTARGUMENT'] = "";
  $postfields['__VIEWSTATE'] = $viewstate;
  $postfields['__EVENTVALIDATION'] = $eventValidation;
  $postfields['ctl00$ContentPlaceHolder1$cblist_quarter$0'] = 'All';
  $postfields['ctl00$ContentPlaceHolder1$ChkHH'] = "checked";
  // Set survey
  $postfields['ctl00$ContentPlaceHolder1$ddl_survey'] = "$surveyid";

  curl_setopt($ch, CURLOPT_POST, 1);
  curl_setopt($ch, CURLOPT_POSTFIELDS, $postfields);
  $ret = curl_exec($ch); // Get result after login page.

  ///////////////////////////


  print("Getting year $year\n");

  $yearno=$year-$allyears[0]+1;

  //print_r($postfields);

  preg_match('~<input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="(.*?)" />~', $ret, $viewstate); 
  preg_match('~<input type="hidden" name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="(.*?)" />~', $ret, $eventValidation); 
  $viewstate = $viewstate[1]; 
  $eventValidation = $eventValidation[1]; 


  $postfields['__EVENTTARGET'] = "";
  $postfields['__EVENTARGUMENT'] = "";
  $postfields['__VIEWSTATE'] = $viewstate;
  $postfields['__EVENTVALIDATION'] = $eventValidation;
  $postfields['ctl00$ContentPlaceHolder1$ddl_data_products']='1';
  $postfields['ctl00$ContentPlaceHolder1$ChkHH']='on';
  $postfields['ctl00$ContentPlaceHolder1$ChkHL']='on';
  $postfields['ctl00$ContentPlaceHolder1$ChkCA']='on';
  $postfields['ctl00$ContentPlaceHolder1$cblist_quarter$0']='None';
  $postfields['ctl00$ContentPlaceHolder1$cblist_quarter$1']='1';
  $postfields['ctl00$ContentPlaceHolder1$cblist_quarter$2']='2';
  $postfields['ctl00$ContentPlaceHolder1$cblist_quarter$3']='3';
  $postfields['ctl00$ContentPlaceHolder1$cblist_quarter$4']='4';
  $postfields['ctl00$ContentPlaceHolder1$cblist_years$'.$yearno]="$year";
  $postfields['__EVENTTARGET']='ctl00$ContentPlaceHolder1$cblist_years$'.$yearno;

  curl_setopt($ch, CURLOPT_POST, 1);
  curl_setopt($ch, CURLOPT_POSTFIELDS, $postfields);
  $ret = curl_exec($ch); // Get result after login page.

  // ==================================================================================================




  //print_r ($postfields);


  preg_match('~<input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="(.*?)" />~', $ret, $viewstate); 
  preg_match('~<input type="hidden" name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="(.*?)" />~', $ret, $eventValidation); 
  $viewstate = $viewstate[1]; 
  $eventValidation = $eventValidation[1]; 
  $postfields['__VIEWSTATE'] = $viewstate;
  $postfields['__EVENTVALIDATION'] = $eventValidation;
  $postfields['ctl00$ContentPlaceHolder1$cblist_Ship$0']='NONE';
  $postfields['__EVENTTARGET']='ctl00$ContentPlaceHolder1$cblist_Ship$0';
  curl_setopt($ch, CURLOPT_POST, 1);
  curl_setopt($ch, CURLOPT_POSTFIELDS, $postfields);
  $ret = curl_exec($ch); // Get result after login page.




  preg_match('~<input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="(.*?)" />~', $ret, $viewstate); 
  preg_match('~<input type="hidden" name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="(.*?)" />~', $ret, $eventValidation); 
  $viewstate = $viewstate[1]; 
  $eventValidation = $eventValidation[1]; 
  $postfields['__VIEWSTATE'] = $viewstate;
  $postfields['__EVENTVALIDATION'] = $eventValidation;
  curl_setopt($ch, CURLOPT_POST, 1);
  curl_setopt($ch, CURLOPT_POSTFIELDS, $postfields);
  $ret = curl_exec($ch); // Get result after login page.



  /* $postfields['ctl00$ContentPlaceHolder1$cblist_Ship$1']='3175'; */
  /* $postfields['ctl00$ContentPlaceHolder1$cblist_Ship$2']='3176'; */
  /* $postfields['ctl00$ContentPlaceHolder1$cblist_Ship$3']='3043'; */
  /* $postfields['ctl00$ContentPlaceHolder1$cblist_Ship$4']='3051'; */
  $postfields['__EVENTTARGET']='ctl00$ContentPlaceHolder1$btn_submit';

  preg_match('~<input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="(.*?)" />~', $ret, $viewstate); 
  preg_match('~<input type="hidden" name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="(.*?)" />~', $ret, $eventValidation); 
  $viewstate = $viewstate[1]; 
  $eventValidation = $eventValidation[1]; 
  $postfields['__VIEWSTATE'] = $viewstate;
  $postfields['__EVENTVALIDATION'] = $eventValidation;
  curl_setopt($ch, CURLOPT_POST, 1);
  curl_setopt($ch, CURLOPT_POSTFIELDS, $postfields);
  $ret = curl_exec($ch); // Get result after login page.

  //$tmp_name = "download.zip";
  $tmp_name = $survey."_".$year.".zip";
  $fd = fopen($tmp_name, 'w');
  fwrite($fd, $ret);
  fclose($fd);

  curl_close($ch);
}


?>
