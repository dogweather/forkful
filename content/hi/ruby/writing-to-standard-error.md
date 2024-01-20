---
title:                "मानक त्रुटि पर लिखना"
html_title:           "Ruby: मानक त्रुटि पर लिखना"
simple_title:         "मानक त्रुटि पर लिखना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामर्स अपनी आईएस कोड के दौरान, अक्सर एरर्स या वर्निंग्स को दूसरी स्थिति में लिखते हैं। यह कोड को एकांतर और आसानी से समझने और दोष खोजने में मदद करता है।

## कैसे करें?
```Ruby
$stderr.puts "यहां आपका एरर या वार्निंग दिखाएंगे।"
```

आप ```puts``` की जगह ```$stderr.puts``` या ```p``` की जगह ```$stderr.print``` का उपयोग करके अपने कोड को स्टैण्डर्ड एरर में लिख सकते हैं।

## गहराई में जाएं
पहले वक्त पर, प्रोग्रामर्स अपने कोड के डीबगिंग के लिए ```puts``` का उपयोग करते थे। लेकिन, जब अपना कोड बहुत बड़ा होता था, तो यह असुविधाजनक हो जाता था। इसलिए, हम अपने कोड के आकार को छोटा रखने के लिए स्टैण्डर्ड एरर में लिखने की प्रक्रिया को अपनाया है। अन्य विकल्पों में इंतेज़ार करने के बजाय, स्टैण्डर्ड एरर में लिखना स्पष्ट और साफ़ होता है।

## और देखें
- [The Ruby Standard Error Class](https://ruby-doc.org/core-3.0.0/StandardError.html)
- [Difference between puts and $stdout.puts](https://stackoverflow.com/questions/16572825/what-is-the-difference-between-puts-and-stdout-puts)