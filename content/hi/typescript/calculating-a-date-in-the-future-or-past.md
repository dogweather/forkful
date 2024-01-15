---
title:                "Bhavishya ya Bhootkal mein ek tithi ka ganana"
html_title:           "TypeScript: Bhavishya ya Bhootkal mein ek tithi ka ganana"
simple_title:         "Bhavishya ya Bhootkal mein ek tithi ka ganana"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों
कभी-कभी हमें एक तारीख को भविष्य में या भूतकाल में गणना करनी होती है। यह गणना समयसीमा और तारीखों को अन्य प्रकार के कामों जैसे दिनों के साथ जोड़ने के लिए उपयोगी हो सकती है।

## कैसे करें
आप TypeScript का उपयोग करके भविष्य या अतीत की तारीख की गणना कर सकते हैं। नीचे दिए गए उदाहरण में, हम यूनिक्स टाइमस्टैम्प को भविष्य या अतीत की तारीख में बदलने के लिए एक फ़ंक्शन बनाएंगे। अपनी तारीख और समयसीमा को ऊपरी बाइंडिंग के माध्यम से पास करें।

```TypeScript
function getDate(date: Date, timePeriod: string) {
  if (timePeriod === "future") {  
    // भविष्य की तारीख के लिए गणना
    const dateInFuture = new Date(date.getTime() + 86400000);  
    // 24 घंटे के समयसीमा जोड़ें
    return dateInFuture;
  } else if (timePeriod === "past") {
    // भूतकाल की तारीख के लिए गणना
    const dateInPast = new Date(date.getTime() - 86400000);  
    // 24 घंटे के समयसीमा से घटाएँ
    return dateInPast;
  }
}

console.log(getDate(new Date(), "future")); // Output: कल की तारीख
```

## गहराई में जाएँ
आप यूनिक्स टाइमस्टैम्प के साथ साफ़-चूना तारीखों को गणना करने के लिए भविष्य और अतीत में अलग-अलग समयसीमाओं का उपयोग कर सकते हैं। आप भविष्य में जाकर तारीखों को गणना करने से पहले, समयसीमा का आकलन करने के लिए आपके पास दो मुख्य विकल्प हैं। पहला विकल्प है समयसीमा को ध्यान से जोड़ना और दूसरा विकल्प है समयसीमा को अमूर