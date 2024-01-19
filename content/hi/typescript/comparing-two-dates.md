---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# TypeScript में दो तारीखों की तुलना कैसे करें

## क्या और क्यों?

तारीख की तुलना करना मतलब होता है दो अलग-अलग दिनों की तुलना करना। प्रोग्रामर इसे करते हैं ताकि वे किसी event की सीमाओं को निर्धारित कर सकें या किसी कार्य को schedule कर सकें।

## कैसे करें:

यहां एक कोड नमूना है जो दो तारीखों की तुलना कैसे करता है:

```TypeScript
let date1 = new Date('2021-01-01');
let date2 = new Date('2021-12-31');

if (date1 < date2) {
    console.log('Date 1 is less than Date 2');
} else if (date1 > date2) {
    console.log('Date 1 is greater than Date 2');
} else {
    console.log('Both dates are equal');
}
```

## गहरा डाइव:

- इतिहासिक context: JavaScript (और इसके SuperSet TypeScript) में दिनांक की तुलना को native रूप से समर्थन मिलता है, जो web application को डेटा प्रबंधित करने में सहायता करता है।
- विकल्प: `getTime` फ़ंक्शन का उपयोग करके भी दो दिनांकों की तुलना की जा सकती है, जो मिलिसेकंड में समय को देता है।
- कार्यान्वयन विवरण: `Date` ऑब्जेक्ट JavaScript में built-in होती है और इसका उपयोग UTC (Coordinated Universal Time) के साथ दिनांक और समय को प्रबंधित करने के लिए किया जाता है।

## अधिक जानकारी:

- [Mozilla TypeScript संदर्भ](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript दिनांक तुलना के बारे में StackOverflow थ्रेड](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)
- [W3schools पर TypeScript के बारे में जानने के लिए](https://www.w3schools.com/typescript/)