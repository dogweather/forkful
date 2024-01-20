---
title:                "स्ट्रिंग को बड़े अक्षरों में बदलना"
html_title:           "Python: स्ट्रिंग को बड़े अक्षरों में बदलना"
simple_title:         "स्ट्रिंग को बड़े अक्षरों में बदलना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

(What & Why?)

'String को Capitalize करना' यानी कि string के पहले character को बड़ा (Capital) करना। यह गर्मजोशी और संदेश के आधिकारिक तौर पर प्रस्तुत करने के लिए प्रोग्रामर्स द्वारा किया जाता है।

## कैसे करें:

(How to:)

Python में, हम `capitalize()` फ़ंक्शन का उपयोग कर सकते हैं। यहां एक उदाहरण है:

```Python
s = "hello world"
print(s.capitalize())
```

आउटपुट:

```Python
Hello world
```

इस code में, `"hello world"` string का पहला character 'h' को 'H' में बदल दिया जाता है।

## गहराई में:

(Deep Dive)

स्ट्रींग को कैपटलाइज़ करने की संभावना शायद Python के पहले ही संस्करण से थी। इस विचार का मुख्य उद्देश्य था कि किसी भी string को आधिकारिक और प्रभावशाली रूप से प्रदर्शित किया जा सकता है।

वैकल्पिक रूप से, आप `title()` फ़ंक्शन को भी उपयोग कर सकते हैं, जिससे string के हर शब्द का पहला अक्षर कैपिटल हो जाता है:

```Python
s = "hello world"
print(s.title())
```

आउटपुट:

```Python
Hello World
```

Python के `str` वर्ग में `capitalize()` फ़ंक्शन को लागू करने से सबसे पहले character को बड़ा कर दिया जाता है और शेष string को छोटा कर दिया जाता है।

## और भी देखें:

(See Also)

1. [Python documentation for str.capitalize()](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
2. [Python documentation for str.title()](https://docs.python.org/3/library/stdtypes.html#str.title)