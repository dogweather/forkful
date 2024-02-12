---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:34:55.070310-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तारीखों की तुलना यानि कि दो तारीखें आपस में कैसे मिलती हैं या कितनी अलग हैं यह देखना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि एप्प्स में डेडलाइंस, इवेंट्स और दूसरी तारीखों को मैनेज करना पड़ता है।

## कैसे करें:
```Lua
-- दो तारीखों का उदाहरण
local date1 = os.time({year=2023, month=4, day=1})  -- 1 अप्रैल 2023
local date2 = os.time({year=2023, month=4, day=15}) --15 अप्रैल 2023

-- तारीखों की तुलना
if date1 < date2 then
    print("तारीख1 तारीख2 से पहले आती है।")
else
    print("तारीख1 तारीख2 के बाद आती है।")
end

-- उदाहरण आउटपुट
-- तारीख1 तारीख2 से पहले आती है।
```

## गहराई में:
तारीखों की तुलना करने के लिए हम `os.time()` फंक्शन का इस्तेमाल करते हैं, जो कि यूनिक्स टाइमस्टैम्प लौटाता है। यूनिक्स टाइमस्टैम्प की शुरुआत 1 जनवरी, 1970 से होती है और यह मापता है कि तब से कितने सेकंड बीत चुके हैं। यह सिस्टम के लिए तारीखों की तुलना करने का मानक तरीका है। वैकल्पिक तरीकों में `os.date()` फंक्शन का इस्तेमाल आता है जो कि विस्तृत तारीख की जानकारी देता है। लेकिन, `os.time()` की सादगी और उच्च स्तरीय प्रयोग के कारण यह ज्यादा पसंद किया जाता है।

## देखें भी:
- Lua मैनुअल के `os.date` और `os.time` सॉफ्ट्वेयर: http://www.lua.org/manual/5.4/manual.html#6.9
- Lua के कम्युनिटी डिस्कशन फोरम: https://www.lua.org/lua-l.html
- समय प्रबंधन और लूआ में संबंधित लाइब्रेरीज़: https://luarocks.org/