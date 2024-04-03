---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:25.357275-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Google Apps Script-\u098F, \u09A4\
  \u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\u09BF \u09A4\u09C1\u09B2\u09A8\u09BE\
  \ \u0995\u09B0\u09BE \u09B9\u09AF\u09BC JavaScript Date \u0985\u09AC\u099C\u09C7\
  \u0995\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\
  , \u09AF\u09BE \u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u0995\u09CB\u09A8\u099F\u09BF \u0986\u0997\u09C7\
  , \u09AA\u09B0\u09C7 \u09AC\u09BE \u09AF\u09A6\u09BF \u09A4\u09BE\u09B0\u09BE \u098F\
  \u0995\u0987 \u09B9\u09AF\u09BC \u09A4\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.539324-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script-\u098F, \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\u09B2\
  \u09BF \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09BE \u09B9\u09AF\u09BC JavaScript\
  \ Date \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7, \u09AF\u09BE \u09A6\u09C1\u099F\u09BF \u09A4\u09BE\
  \u09B0\u09BF\u0996\u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0995\u09CB\u09A8\
  \u099F\u09BF \u0986\u0997\u09C7, \u09AA\u09B0\u09C7 \u09AC\u09BE \u09AF\u09A6\u09BF\
  \ \u09A4\u09BE\u09B0\u09BE \u098F\u0995\u0987 \u09B9\u09AF\u09BC \u09A4\u09BE \u09A8\
  \u09BF\u09B0\u09CD\u09A7\u09BE\u09B0\u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09AE\u09BE\u09A8\u0995 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\
  \u09C1\u09B2\u09BF\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09B8\u09AE\
  \u09CD\u09AD\u09AC \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u09AA\u09CD\u09B0\u0995\u09CD\
  \u09B0\u09BF\u09AF\u09BC\u09BE \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
Google Apps Script-এ, তারিখগুলি তুলনা করা হয় JavaScript Date অবজেক্ট ব্যবহার করে, যা দুটি তারিখের মধ্যে কোনটি আগে, পরে বা যদি তারা একই হয় তা নির্ধারণ করার জন্য মানক পদ্ধতিগুলির ব্যবহার সম্ভব করে। এখানে একটি বেসিক প্রক্রিয়া রয়েছে:

```javascript
function compareDates() {
  var date1 = new Date('2023-04-01T00:00:00');
  var date2 = new Date('2023-04-15T00:00:00');

  // তারিখগুলি তুলনা করুন
  if (date1 < date2) {
    Logger.log('Date1 Date2-এর আগে হয়');
  } else if (date1 > date2) {
    Logger.log('Date1 Date2-এর পরে হয়');
  } else {
    Logger.log('উভয় তারিখই একই');
  }
}

// নমুনা আউটপুট:
// Date1 Date2-এর আগে হয়
```

আরো বিস্তারিত তুলনার জন্য (যেমন দুটি তারিখের মধ্যে দিনের সংখ্যা), আপনি এক তারিখের থেকে অন্য তারিখ বিয়োগ করতে পারেন, যা মিলিসেকেন্ডে পার্থক্য ফেরত দেয়:

```javascript
function daysBetweenDates() {
  var date1 = new Date('2023-04-01');
  var date2 = new Date('2023-04-15');
  
  var difference = date2 - date1;
  
  var days = difference / (1000 * 60 * 60 * 24); // মিলিসেকেন্ডকে দিনে পরিণত করুন
  Logger.log(days + ' দিন তারিখের মধ্যে');
}

// নমুনা আউটপুট:
// 14 দিন তারিখের মধ্যে
```

## গভীর ডুব
Google Apps Script তারিখের তুলনার জন্য JavaScript Date অবজেক্টের মূলনীতিগুলি ব্যবহার করে, যা ভাষার শুরু থেকেই একটি মৌলিক দিক হয়ে আসছে। Unix Epoch (১ জানুয়ারী, ১৯৭০) থেকে মিলিসেকেন্ড হিসেবে তুলনামূলক মান ব্যবহার করা, তারিখগুলির মধ্যে পার্থক্য বা সাদৃশ্য নির্ধারণে উচ্চ স্তরের নির্ভুলতা প্রদান করে।

যদিও Google Apps Script-এর পরিধির মধ্যে বেশিরভাগ ব্যবহারের জন্য এই পদ্ধতি কার্যকর, তারিখের উপরে অপারেশনগুলি — যেমন টাইমজোন সংশোধন এবং লিপ বছর গণনা — কখনও কখনও বিভ্রান্তিকর হতে পারে। অন্যান্য প্রোগ্রামিং পটভূমিযুক্ত ডেভেলপাররা (যেমন Python, যেখানে `datetime` এবং `dateutil` মডিউলগুলি তারিখের আরও নান্দনিক হ্যান্ডলিং প্রদান করে) হয়তো JavaScript Date অবজেক্টকে ফিচারের দিক থেকে ঘাটতি মনে করতে পারেন।

সাধারণ তুলনার বাইরে জটিল তারিখ হ্যান্ডলিং এবং ম্যানিপুলেশনের জন্য, `Moment.js` এর মতো লাইব্রেরিগুলি (যা বাহ্যিক APIs এর মাধ্যমে Google Apps Script-এর মধ্যে এখনও ব্যবহার করা যেতে পারে) এমন সম্পদশালী সেট অফ কার্যকারিতা অফার করে যা এই ঘাটতিগুলি মোকাবিলা করে। তবে, মূল JavaScript Date অবজেক্ট, বিশেষ করে Google Apps Script এবং এর Google-এর অ্যাপস সমূহের সাথে সমন্বয়ের প্রেক্ষাপটে, বেশিরভাগ তারিখ তুলনা কাজের জন্য একটি নির্ভরযোগ্য টুল হিসাবে কাজ করতে থাকে।
