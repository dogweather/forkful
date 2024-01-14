---
title:    "Elm: भविष्य या भूतकाल में एक दिनांक की गणना"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों

किसी भी प्रोग्रामिंग कार्य में तारीख की परिभाषा समझना अत्यावश्यक होता है। एल्म (Elm) में तारीख के साथ काम करना भी महत्वपूर्ण है, और समय को नियंत्रित रूप से बनाना काफी सरल है। इस ब्लॉग पोस्ट में हम एल्म में तारीख कैसे गणना कर सकते हैं, वह भी भविष्य और भूतकाल में, इसके बारे में जानेंगे।

# कैसे करें

"```Elm
Date.fromTime 0
    |> Date.day |> toString -- वर्तमान दिन का नाम
    "Friday"

Date.fromTime 0
    |> Date.day |> Date.nextDay |> toString -- अगले दिन का नाम
    "Saturday"

Date.fromTime 0
    |> Date.day |> Date.inMonth Date.July |> toString -- भविष्य में गणना की गई माह का नाम
    "July"

Date.fromTime 0
    |> Date.day |> Date.inWeek 2 |> toString -- भूतकाल में गणना की गई हफ्ते का नाम
    "Tuesday"
"```

# गहराई जानें

एल्म में तारीख को गणना करने के लिए हम `Date.fromTime` का उपयोग कर सकते हैं। यह फंक्शन समय को एक मिलीसेकंड की श्रृंखला में लेता है और उसे `Date` डेटा टाइप में वापस देता है। इसके बाद हम `Date` टाइप के और अन्य फंक्शन्स का उपयोग कर सकते हैं। हम भविष्य और भूतकाल में तारीख को गणना करने का उदाहरण भी दे रहे हैं।

# देखें भी

- [Date module documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Learn Elm in Hindi](https://www.youtube.com/playlist?list=PLnoB1G2wCLOmOkf_DzK0UUSdYdL1KuqW2)
- [Official Elm website](https://elm-lang.org/)