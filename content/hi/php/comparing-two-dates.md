---
title:                "हिंदी में दो तारीखों को तुलना करना."
html_title:           "PHP: हिंदी में दो तारीखों को तुलना करना."
simple_title:         "हिंदी में दो तारीखों को तुलना करना."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

दो दिनांकों को तुलना करने के कारण एक समस्या को हल करने में मदद मिलती है।

## कैसे करें

```
<?php

// दो तारीखों को तुलना करने के लिए फ़ंक्शन कोड
function compareDates($date1, $date2) {
  // दो तारीखों को strtotime function का उपयोग करके unix timestamp में बदलें
  $timestamp1 = strtotime($date1);
  $timestamp2 = strtotime($date2);

  // तुलना करें और समान होने पर संख्या को वापस करें
  if ($timestamp1 === $timestamp2) {
    return 0;
  } else if ($timestamp1 > $timestamp2) {
    return 1;
  } else {
    return -1;
  }
}

// उदाहरण: दो तारीखों को तुलना करें
$date1 = "5 October 2020";
$date2 = "10 October 2020";

// फ़ंक्शन का उपयोग करें
$result = compareDates($date1, $date2);

// समानता के आधार पर उत्पादन दें
if ($result === 0) {
  echo "$date1 और $date2 बराबर हैं।";
} else if ($result === 1) {
  echo "$date1 $date2 से पहले है।";
} else {
  echo "$date1 $date2 के बाद है।";
}

?>
```

**आउटपुट:** 5 October 2020 और 10 October 2020 के बीच अंतर है।

## गहराई से जाने

दो दिनांकों को तुलना करने के लिए सबसे सामान्य विधि strtotime() function का उपयोग है। यह फ़ंक्शन दो स्वरूपों में उपलब्ध है - स्ट्रिंग या टाइमस्टैंप। कई आधारभूत समकालीन फ़ंक्शनें फ़ंक्शन के आंकड़े को दो तारीखों को तुलना करने के लिए उपयोग कर सकती हैं।

## देखें भी

[Solve a variety of tasks involving comparing dates in PHP](https://www.geeksforgeeks.org/php-comparing-two-dates/) 
[PHP DateTime class for more advanced date and time operations](https://www.php.net/manual/en/class.datetime.php)