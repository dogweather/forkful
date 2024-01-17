---
title:                "Json के साथ काम करना"
html_title:           "PHP: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON के साथ काम करना वह विशेष कौशल है जो कोड को डेटा को संपादित करने और प्रसंस्करण करने की अनुमति देता है। प्रोग्रामर्स इसलिए JSON को उपयोग करते हैं आजकल कि यह सुपर संवेदनशील, अत्यंत ही उपयोगी, और स्क्रिप्टिंग भाषाओं के साथ प्रभावी तरीके से इंटीग्रेट होता है।

## कैसे:

```PHP
// JSON डेटा बनाएं
$data = array(
  "पहला" => "खुद",
  "दूसरा" => "समझने",
  "तीसरा" => "जगह",
  "चौथा" => "Json"
);

// PHP के साथ JSON बनाएं
$jsondata = json_encode($data);

// JSON डेटा को पढ़ें
$jsonobj = json_decode($jsondata);

// डेटा के रूप में प्रिंट करें
echo $jsonobj->पहला . ", " . $jsonobj->दूसरा . ", " . $jsonobj->तीसरा . ", " . $jsonobj->चौथा . "!";

// Output: खुद, समझने, जगह, Json!
```

## गहराई में जाएं:

JSON का विस्तृत समर्थन 1990 के दशक में के उदगम का हिस्सा था। इसके अलावा, कुछ अन्य विकल्पों के रूप में XML और YAML भी प्रचलित हैं। PHP में, ```json_encode``` और ```json_decode``` फंक्शन को उपयोग करके JSON डेटा को प्रोसेस करना सुनिश्चित होता है।

## इससे जुड़े देखें:

- [PHP Manual: JSON फंक्शन](https://www.php.net/manual/en/book.json.php)
- [W3Schools: PHP JSON उदाहरण](https://www.w3schools.com/php/php_json_example.asp)
- [What is JSON? एक सरल गाइड](https://www.digitalocean.com/community/tutorials/an-introduction-to-json)