---
title:                "PHP: दो तारीखों की तुलना करना"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

# क्यों

कभी-कभी प्रोग्रामिंग में हमें दो तारीखों को तुलना करनी पड़ती है। इससे हम आसानी से पता कर सकते हैं कि कौन सी तारीख आगे है और कौन सी पीछे है। अधिक जानकारी के लिए पढ़ते रहिए।

# कैसे करें

आप `strtotime()` फ़ंक्शन का उपयोग करके दो तारीखों को timestamp में प्रकाशित कर सकते हैं। तारीख से परिणाम को subtract कर जब उसे तुलना करें तो हमें पता चलेगा कि कौन सी तारीख आगे है।

```PHP
$date1 = strtotime("2020-01-01");
$date2 = strtotime("2020-01-05");

// Finding the difference in seconds
$difference = $date2 - $date1;

// Converting seconds to days
$days = $difference / (60 * 60 * 24);

// Only 1-2 sentences explaining why someone would engage in comparing two dates.
echo "The difference is " . $days . " days.";
```

आउटपुट:
> The difference is 4 days.

# गहराई में कूदें

इस तरह के तारीख समीकरण से आप समझेंगे कि strtotime() फ़ंक्शन कैसे काम करता है और कैसे आप दो तारीखों के बीच समय अंतर निकाल सकते हैं। इसके लिए PHP की आधिकारिक डॉक्यूमेंटेशन भी पढ़ सकते हैं।

## See Also

- [PHP official documentation on strtotime() function](https://www.php.net/manual/en/function.strtotime.php)
- [Difference between two dates in PHP by Shalini Chaudhary](https://www.codementor.io/@shalinchoksi/comparing-two-dates-using-datetime-functions-in-php-6mrbf8wk8)
- [Comparing dates in PHP by Nandini Nama](https://www.geeksforgeeks.org/comparing-dates-in-php/)