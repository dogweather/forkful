---
title:    "TypeScript: भविष्य या भूतकाल में तारीख की गणना करना"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

किसी को भविष्य या भूतकाल में दिनांक की गणना करने की क्षमता अपने को स्वयं परिभाषित हो सकता है।

## कैसे करें

`````TypeScript
// एक दिनांक से दूरी का गणना करने के लिए:
let currentDate: Date = new Date();
let futureDate: Date = new Date(currentDate.getTime() + (30 * 24 * 60 * 60 * 1000)); // 30 दिन पहले का दिनांक
console.log(futureDate); // इसका आउटपुट 30 दिन के आगे की तारीख होगी

// दो दिनों के बीच दिनांक की गणना करने के लिए:
let pastDate: Date = new Date(currentDate.getTime() - (2 * 24 * 60 * 60 * 1000)); // 2 दिन पहले का दिनांक
console.log(pastDate); // इसका आउटपुट 2 दिन पहले की तारीख होगी
`````

## गहराई में जा

जब हम एक दिनांक से दूर या पीछे की गणना करते हैं, तो हम इसे मिलीसेकंड्स में करते हैं। हम इसे अपनी आवश्यकतानुसार मिनट, घंटे, दिन या साल में भी बदल सकते हैं। इसके लिए हम समय की संख्या में गुणा या बाँट कर सकते हैं। अधिक गहराई में जाने के लिए, आप [यहाँ](https://www.w3schools.com/js/js_date_methods.asp) से और अधिक जानकारी प्राप्त कर सकते हैं।

## देखें भी

- [Date ऑब्जेक्ट](https://www.w3schools.com/js/js_date_objects.asp)
- [Date और Time समस्याएं](https://www.w3schools.com/js/js_datetime.asp)