---
title:    "Java: उपयोग करके नियमित अभिव्यक्तियों का उपयोग"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

Regular expressions उन users के लिए उपयोगी हो सकते हैं जो अपने टेक्स्ट स्ट्रिंग्स में निश्चित प्रकार की डेटा को ढूंढना चाहते हैं या उन्हें उन डेटा को manipulate करना होता है।

## कैसे करें

### बुनियादी प्रकार का स्ट्रिंग मैच करना

```Java
String regex = "he..o";
String input = "hello";
Pattern pattern = Pattern.compile(regex);
Matcher matcher = pattern.matcher(input);
if (matcher.find()) {
  System.out.println("String matched");
} else {
  System.out.println("String didn't match");
}
```

आउटपुट:

`String matched`

### अंतर्जात्रिक कोड स्पैस निकालना

```Java
String input = "Language: जावा";
String output = input.replaceAll("[^\\p{L}\\p{Nd}]+", "");
System.out.println(output);
```

आउटपुट:

`Languageजावा`

## गहराई में जाने के लिए

Regular expressions के बारे में अधिक जानने के लिए, आप इन लिंक्स को देख सकते हैं:

- [Java Regular Expressions Tutorial](https://www.javatpoint.com/java-regex)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Regex Tester](https://regex101.com/)

## अन्य लिंक्स

- [जावा क्लास और ऑब्जेक्ट का अध्ययन करें](https://www.studytonight.com/java/class-and-object.php)
- [जावा स्ट्रिंग क्लास चिजे पढ़ें](https://www.javatpoint.com/java-string)