---
title:                "टेक्स्ट को ढूंढ़ना और बदलना"
html_title:           "Haskell: टेक्स्ट को ढूंढ़ना और बदलना"
simple_title:         "टेक्स्ट को ढूंढ़ना और बदलना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट को खोजना और बदलना हमारे दैनिक जीवन में काफी आम होता है, और यह विशेष रूप से डेवलपर्स के लिए एक अहम काम है। जब हमें एक बड़ी फ़ाइल में विशेषताओं को ढूंढना और उसको बदलना होता है, तो हम इस टूल का उपयोग करते हैं। यह हमें समय और परेशानी दोनों बचाता है।

## कैसे करें?

```Haskell
main = do
  -- "Hello World!" को "नमस्ते दुनिया!" में बदलें।
  let str = "Hello World!"
  putStrLn $ replace "Hello World!" "नमस्ते दुनिया!" str

  -- "नमस्ते दुनिया!" से "Hello World!" में बदलें।
  putStrLn $ replace "नमस्ते दुनिया!" "Hello World!" str
 where
  replace = flip . replace_trans

replace_trans sfinds srep str = let slen = length sfinds
   flist = head $ filter ((sfinds ==) . flip take str) $ inits str
   (mlist, rest) = splitAt (length flist + slen) str
   prepend = id
      . if null flist
        then id
        else ((++) flist)
   rest' = if rest == sfinds then srep else rest
 in prepend rest'
```

आउटपुट:

```Haskell
नमस्ते दुनिया!
Hello World!
```

## गहराई में जाएं

टेक्स्ट को खोजने और बदलने की इस तकनीक को 1970 के दशक में केन थॉम्पसन द्वारा उपनिर्धारित किया गया था। वैकल्पिक और अधिक उन्नत तरीकों में शामिल हैं फ़ाइल परिवर्तन और कांट्रोल संरचनाओं के साथ प्रोग्राम में टेक्स्ट को बदलना। यह जिंदगी को सरल बनाता है जो एक विशेष उदाहरण देखने से स्पष्ट हो जाता है।

## भी देखें

- [Haskell टेक्स्ट प्रोग्रामिंग गाइड](https://wiki.haskell.org/Introduction_to_Haskell_Text_IO)
- [टेक्स्ट में खोजें और बदलें डेवलपर्स का ट्यूटोरियल](https://www.twilio.com/blog/2015/02/comprehensive-guide-updating-text-data-in-haskell-prelude-text.html)