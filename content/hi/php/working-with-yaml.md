---
title:                "PHP: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-yaml.md"
---

{{< edit_this_page >}}

# क्यों

यामल (YAML) क्यों इतना महत्वपूर्ण है, और एक प्रोग्रामर के लिए इससे क्या लाभ है? यामल सार्वजनिक डेटा को स्ट्रक्चर्ड फॉर्मेट में स्टोर करने का एक अत्यंत सरल और सुविधाजनक तरीका है। इससे डेटा को संगठित रूप से अलग किया जा सकता है जो प्रोग्रामिंग में अहम बात है।

# कैसे करें

```PHP
// YAML फ़ाइल से डेटा लोड करें
$data = yaml_parse_file('data.yaml');

// डेटा के फ़ील्ड एक्सेस करें
$name = $data['name'];
$age = $data['age'];

// प्रिंट करें
echo "नाम: $name \n";
echo "आयु: $age \n";
```

आउटपुट:
```
नाम: जॉन डो
आयु: 25
```

# गहराईशोध

यामल काफी सरल सिंटैक्स वाला है जिससे आप अपने डेटा को हीरार्कील रूप में आसानी से संगठित कर सकते हैं। इसकी मुख्य उपयोगिता यह है कि आप डेटा को अलग प्रकार की फ़ाइलों में स्टोर कर सकते हैं और उन्हें प्रोग्रामिंग में आसानी से यूज कर सकते हैं। इसमें कई भाषाओं के साथ संगठित डेटा सपोर्ट है जैसे PHP, Python, Perl और अन्य। आप और भी गहराई में YAML के बारे में पढ़कर अपनी प्रोग्रामिंग कौशल को बढ़ा सकते हैं।

# भीड़भाड़

# अन्य लिंक
- [YAML में बेसिक सिंटैक्स की जानकारी](https://www.tutorialspoint.com/yaml/yaml_basics.htm)
- [YAML पोर्टल](https://yaml.org/)
- [YAML का GitHub रिपोजिटरी](https://github.com/yaml)
- [YAML में हैंडलिंग एरर्स को समझना](https://www.toptal.com/php/yaml-tutorial-for-php)