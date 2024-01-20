---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग इंटरपोलेशन का अर्थ है कि किसी स्ट्रिंग के भीतर उसकी मूल्य वाली व्यतिरेक्ता स्थल डालना। प्रोग्रामर इसका उपयोग यह सुनिश्चित करने के लिए करते हैं कि कोड में स्पष्टता और पठनीयता बनी रहे।

## कैसे करें:

TypeScript में, हम बैकटिक्स(```) का उपयोग करके इंटरपोलेशन को कर सकते हैं। निम्नलिखित कोड दिखा रहा है कि कैसे:

```TypeScript
const name = "Viraj";
const message = `नमस्कार, ${name}!`;
console.log(message); // नमस्कार, Viraj!
```

यहां `${name}` में स्थान होता है जहाँ `name` वेरिएबल का मूल्य स्थापित होता है।

## डीप डाइव:

स्ट्रिंग इंटरपोलेशन का उपयोग पहले से ही कुछ भाषाओं में किया जा रहा है, जैसे कि Perl और Ruby। TypeScript, जो JavaScript का एक सुपरसेट है, ने ES6 के साथ इस सुविधा को जोड़ा। 

विकल्प रूप में, आप `+` ऑपरेटर का उपयोग करके स्ट्रिंग को मिला सकते हैं, लेकिन यह जटिलता और त्रुटि की संभावना बढ़ा सकता है।

स्ट्रिंग इंटरपोलेशन के पिछे का प्रमुख विचार यह है कि यह टेम्पलेट स्ट्रिंग लेता है और उसे रनटाइम पर आवंटित करता है।

## अधिक देखें:

- [MDN Web Docs: Template literals(शब्दों के साँचे)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)