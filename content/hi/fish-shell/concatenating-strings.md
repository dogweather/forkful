---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वाक्यांशों को जोड़ने का तत्व "concatenating strings" हमें एक ही लाइन में विभिन्न चर या वाक्यांशों को जोड़ने की अनुमति देता है। प्रोग्रामर इसे कोडिंग समय बचाने, यातायात कम करने और कोड अधिक पठनीय बनाने के लिए करते हैं।

## कैसे करें:

Fish shell में, आप set command का उपयोग करके और जोड़ने की गणना करके वाक्यांशों को जोड़ सकते हैं। तब तक गणना जारी रखने के लिए, संकलन में whitespace शामिल नहीं होता है। इसका नमूना नीचे है।

```fish 
set var1 "नमस्ते, "
set var2 "विश्व!"
echo $var1$var2
```

उत्तर में हमें मिलेगा "नमस्ते, विश्व!".

## गहरा डाइव:

Concatenating strings का इतिहास अनगिनत प्रोग्रामिंग भाषाओं के साथ खुद का पतन करता है, लेकिन मूलतः इसे शून्यइकाईयों की कमी को दूर करने के लिए विकसित किया गया था। "string interpolation" और "string formatting" अभी भी एक महत्वपूर्ण विकल्प हैं। Fish shell implementation "Whitespace" को नजरबंद करता है, ध्यान दें कि कुछ भाषाएं इसे शामिल करती हैं।

## देखें भी:

[कनकाटेनेशन के बारे में अधिक जानकारी के लिए Python डॉक्यूमेंटेशन पढ़ें](https://docs.python.org/3/reference/lexical_analysis.html#string-literal-concatenation)

[Fish Shell कैसे स्ट्रिंग्स को जोड़ता है, इसकी विस्तृत जानकारी के लिए](https://fishshell.com/docs/current/index.html)

[JavaScript में वाक्यांशों को जोड़ने की विभिन्न तकनीकों पर ताजगी के लिए MDN का गाइड पढ़ें](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)