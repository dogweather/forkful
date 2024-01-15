---
title:                "Bhavishya ya Bhoot mein ek tithi ka ganana"
html_title:           "PHP: Bhavishya ya Bhoot mein ek tithi ka ganana"
simple_title:         "Bhavishya ya Bhoot mein ek tithi ka ganana"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

किसी भी उलझन से उत्थित होने से पहले हमें अपनी योजना के अनुसार आगामी या भूतकाल की तारीख जानने की आवश्यकता हो सकती है। इसलिए, PHP में तारीख को भविष्य में या भूतकाल में गणित करना अत्यंत महत्वपूर्ण है।

## कैसे करें

PHP में तारीख को भविष्य में या भूतकाल में गणित करने के लिए हम `strtotime()` और `date()` फ़ंक्शन का इस्तेमाल कर सकते हैं। जो तारीख को स्ट्रिंग में दिए गए फ़ॉर्मेट में लिखेगा और उसे फिर से तारीख के रूप में फिर से फॉर्मेट करेगा। नीचे दिए गए कोड स्निपेट द्वारा आप यह बेहद आसानी से देख सकते हैं।

```PHP
// भविष्य की तारीख गणित करने के लिए
$future_date = strtotime('+1 week'); // +1 week का मतलब है कि इसके बाद के हफ्ते की तारीख हमें मिल जाएगी
echo date('F j, Y', $future_date); // तारीख को नई फ़ॉर्मेट में प्रिंट करें

// भूतकाल की तारीख गणित करने के लिए
$past_date = strtotime('-2 months'); // -2 months का मतलब है कि इससे दो महीने पहले की तारीख निकालनी है
echo date('F j, Y', $past_date); // तारीख को नई फ़ॉर्मेट में प्रिंट करें
```

**आउटपुट:**

```output
October 2, 2021 // भविष्य की तारीख की उदाहरण
August 2, 2021 // भूतकाल की तारीख की उदाहरण
```

## गहराई में

तारीख को भविष्य में या भूतकाल में गणित करना बहुत उपयोगी हो सकता है। इससे हमें उचित समय प्रबंधन करने में मदद मिलत