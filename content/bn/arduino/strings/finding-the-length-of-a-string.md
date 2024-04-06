---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:16.481981-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09C1\u09B0\u09BE\u09A8\
  \u09CB \u09A6\u09BF\u09A8\u09C7, C \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE `<string.h>` \u09A5\u09C7\u0995\u09C7 `strlen()`\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 \u09A8\u09BE\u09B2-\u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09C7\
  \u099F\u09B0 \u09AA\u09B0\u09CD\u09AF\u09A8\u09CD\u09A4 \u0985\u0995\u09CD\u09B7\
  \u09B0 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09A4\u0964 Arduino-\u09B0 \u099C\u0997\
  \u09A4\u09C7, `String` \u0995\u09CD\u09B2\u09BE\u09B8\u2026"
lastmod: '2024-04-05T22:51:05.545216-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09C1\u09B0\u09BE\u09A8\u09CB \u09A6\u09BF\u09A8\u09C7, C \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE `<string.h>`\
  \ \u09A5\u09C7\u0995\u09C7 `strlen()` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A8\u09BE\u09B2-\u099F\u09BE\
  \u09B0\u09CD\u09AE\u09BF\u09A8\u09C7\u099F\u09B0 \u09AA\u09B0\u09CD\u09AF\u09A8\u09CD\
  \u09A4 \u0985\u0995\u09CD\u09B7\u09B0 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09A4\
  \u0964 Arduino-\u09B0 \u099C\u0997\u09A4\u09C7, `String` \u0995\u09CD\u09B2\u09BE\
  \u09B8 `length()` \u09AE\u09C7\u09A5\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u099C\u09C0\u09AC\u09A8 \u09B8\u09B9\u099C \u0995\u09B0\u09C7 \u09A6\u09C7\u09DF\
  \u0964 \u09A4\u09AC\u09C7 \u09AE\u09A8\u09C7 \u09B0\u09BE\u0996\u09AC\u09C7\u09A8\
  , \u09B8\u09C0\u09AE\u09BF\u09A4 \u09AE\u09C7\u09AE\u09B0\u09BF \u09B8\u09AE\u09CD\
  \u09AA\u09A6 \u09B8\u09AE\u09DF\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09BE\
  \u09A5\u09C7 `String` \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09B2\u09C7 \u09AB\u09CD\u09B0\u09BE\u0997\
  \u09AE\u09C7\u09A8\u09CD\u099F \u09B9\u09DF\u09C7 \u09AF\u09C7\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7\u0964 \u09AC\u09BF\u0995\u09B2\u09CD\u09AA?"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09B0 \u09A6\u09C8\u09B0\
  \u09CD\u0998\u09CD\u09AF \u099A\u09BF\u09B9\u09CD\u09A8\u09BF\u09A4 \u0995\u09B0\
  \u09BE"
weight: 7
---

## কিভাবে:
```Arduino
void setup() {
  Serial.begin(9600); // সিরিয়াল যোগাযোগ শুরু করা
  String myString = "Hello, Arduino!"; // আপনার স্ট্রিং এখানে
  int stringLength = myString.length(); // স্ট্রিং-এর দৈর্ঘ্য নির্ণয়
  Serial.print("The length of the string is: ");
  Serial.println(stringLength); // দৈর্ঘ্য প্রদর্শন
}

void loop() {
  // এখানে কিছু করার নেই।
}
```
নমুনা আউটপুট:
```
স্ট্রিং-এর দৈর্ঘ্য হল: 15
```

## গভীর ডুব
পুরানো দিনে, C প্রোগ্রামাররা `<string.h>` থেকে `strlen()` ফাংশন ব্যবহার করে নাল-টার্মিনেটর পর্যন্ত অক্ষর গণনা করত। Arduino-র জগতে, `String` ক্লাস `length()` মেথডের সাথে জীবন সহজ করে দেয়। তবে মনে রাখবেন, সীমিত মেমরি সম্পদ সময়ের সাথে সাথে `String` অবজেক্ট ব্যবহার করলে ফ্রাগমেন্ট হয়ে যেতে পারে। বিকল্প? চার এরে (C-স্টাইল স্ট্রিং), যা মেমরির পক্ষে বেশি বান্ধব কিন্তু পরিচালনা করা ট্রিকি।

বৃহত্তর প্রকল্পগুলির জন্য, সবসময় মেমরি ম্যানেজমেন্ট বিবেচনা করুন। `length()` মেথডে কোনো অতিরিক্ত কম্পিউটিং প্রয়োজন হয় না—`String` অবজেক্ট নিজের আকারের হিসাব রাখে। অভ্যন্তরীণভাবে, `length()` একটি দ্রুত লুক-আপ, অক্ষর গণনা নয়। সেটি দক্ষ! কিন্তু, যদি আপনি মেমরি সম্পদে কম থাকেন, তবে চার এরে এবং ম্যানুয়াল দৈর্ঘ্য গণনা দিয়ে পুরানো দিনের `strlen()` এ ফিরে যান।

## আরো দেখুন
- Arduino `String` রেফারেন্স: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Arduino `strlen()` ফাংশন C-স্টাইল স্ট্রিংগুলির জন্য: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- Arduino-তে `String` এবং চার এরে নিয়ে আলোচনা: https://forum.arduino.cc/t/string-vs-char-array/678207
