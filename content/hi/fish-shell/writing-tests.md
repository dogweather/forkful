---
title:                "Fish Shell: टेस्ट लिखना"
simple_title:         "टेस्ट लिखना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

तुम कोई भी programming language सीखते हो तो tests का महत्व काफी अधिक होता है। किसी application को मुकामल बनाने से पहले tests लिखना आपके code को स्पष्ट और reliable बनाएगा और आपको बग्स को ठीक करने में मदत करेगा।

## कैसे करें

```Fish Shell
# उदाहरण
echo "नमस्ते, दुनिया!"
# आउटपुट: नमस्ते, दुनिया!
```

टेस्ट लिखने के लिए, आपको `Fish Shell` का उपयोग करके `assert` कमांड को जोड़ना होगा। यह कमांड दो values को compare करता है और अगर वे equal नहीं हैं तो एक error message देता है। नीचे दिए गए example को follow करके आप टेस्ट कर सकते हो:

```Fish Shell
begin
  assert 1 -eq 2 "1 should equal 2"
end
```

## गहराई में जाएं

टेस्ट लिखने में गहराई में जाने से आपको टेस्ट के बारे में और अधिक जानकारी होगी। आप अलग-अलग data types, functions और loops को टेस्ट कर सकते हो। आपको अपने code को organize करके उनके परीक्षण के लिए reusable tests लिखने की सलाह दी जाती है। आप अपने code के साथ संबंधित issues को ट्रैक करने के लिए test suites भी बना सकते हो।

## देखें भी

- [Fish Shell का documentation](https://fishshell.com/docs/current/)
- [Fish Shell से सम्बंधित tutorials](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell और test suites](https://fishshell.com/docs/current/tutorial.html#advanced-testing-with-suite)