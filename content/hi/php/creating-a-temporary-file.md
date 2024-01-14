---
title:                "PHP: Atemporary file बनाना"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

आज के युग में प्रोग्रामिंग स्किल्स का बहुत महत्व हैं और उन्हें सीखने और अपनाने में कोई भी व्यक्ति नहीं पीछे रहना चाहता हैं। एक ऐसी स्किल्ड में से एक हैं PHP प्रोग्रामिंग। आप अपने PHP कोड को और भी सुरक्षित और बेहतर बनाने के लिए टेम्पोररी फाइल सृजन कर सकते हैं। इसका प्रयोग आपको सुरक्षित डेटा जोड़ने या और भी अनेक विभिन्न कार्यों में मदद कर सकता हैं।

## कैसे करें

```PHP
$file = tmpfile();
fwrite($file, "This is a temporary file.");
echo fread($file, filesize("Your File Path"));
fclose($file);
```

## गहराई तक जाईये

टेम्पोररी फाइल सृजन करने के बारे में गहराई से जानने से पहले हमें यह समझना चाहिए कि यह क्या है और यहाँ इसका क्या उपयोग होता हैं। टेम्पोररी फाइलें अस्थायी फाइलें होती हैं जो आमतौर पर एप्लीकेशन या सिस्टम प्रोसेसेस की अपने आप डिलीट हो जाती हैं। ये फाइलें ऑपरेशन कमलाई और सुरक्षितता बनाए रखने के लिए स्टोर होती हैं। इससे अपने कोड को प्रोटेक्ट किया जा सकता हैं और डेटा लॉस या वाइरस से बचाया जा सकता हैं।

## अधिक जानकारी के लिए

[PHP Official Documentation on tmpfile() function](https://www.php.net/manual/en/function.tmpfile.php)

[GeeksforGeeks tutorial on PHP temporary files](https://www.geeksforgeeks.org/creating-temporary-files-in-php/)

## देखें भी

[How to Secure Your PHP Code](https://www.example.com/how-to-secure-php-code)

[Introduction to PHP: Basics and Best Practices](https://www.example.com/introduction-to-php-basics)