---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:05.466850-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A7\u09B0\u09C1\u09A8, \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\u09C7 \u098F\u0995\u099F\u09BF \u09B8\
  \u09BE\u09A7\u09BE\u09B0\u09A3 \u099C\u09BE\u09AD\u09BE \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE \u0986\u099B\u09C7 \u09AF\u09BE \u09A0\u09BF\u0995\
  \u09AE\u09A4\u09CB \u0995\u09BE\u099C \u0995\u09B0\u099B\u09C7 \u09A8\u09BE, \u098F\
  \u09AC\u0982 \u0986\u09AA\u09A8\u09BF \u0995\u09BE\u09B0\u09A3 \u09AC\u09C1\u099D\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u099B\u09C7\u09A8 \u09A8\u09BE\u0964 \u098F\u0996\
  \u09BE\u09A8\u09C7 \u0995\u09C0\u09AD\u09BE\u09AC\u09C7 \u099C\u09BE\u09AD\u09BE\
  \ \u09A1\u09C7\u09AD\u09C7\u09B2\u09AA\u09AE\u09C7\u09A8\u09CD\u099F\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:43.909040-06:00'
model: gpt-4-0125-preview
summary: "\u09A7\u09B0\u09C1\u09A8, \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\
  \u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u099C\u09BE\
  \u09AD\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u0986\u099B\
  \u09C7 \u09AF\u09BE \u09A0\u09BF\u0995\u09AE\u09A4\u09CB \u0995\u09BE\u099C \u0995\
  \u09B0\u099B\u09C7 \u09A8\u09BE, \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF \u0995\
  \u09BE\u09B0\u09A3 \u09AC\u09C1\u099D\u09A4\u09C7 \u09AA\u09BE\u09B0\u099B\u09C7\
  \u09A8 \u09A8\u09BE\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09C0\u09AD\u09BE\
  \u09AC\u09C7 \u099C\u09BE\u09AD\u09BE \u09A1\u09C7\u09AD\u09C7\u09B2\u09AA\u09AE\
  \u09C7\u09A8\u09CD\u099F\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u099C\u09A8\u09AA\
  \u09CD\u09B0\u09BF\u09AF\u09BC IDE \u098F\u0995\u099F\u09BF, \u0987\u0995\u09CD\u09B2\
  \u09BF\u09AA\u09CD\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u098F\u0995\u099F\u09BF \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u099A\
  \u09BE\u09B2\u09C1 \u0995\u09B0\u09AC\u09C7\u09A8."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
ধরুন, আপনার কাছে একটি সাধারণ জাভা প্রোগ্রাম আছে যা ঠিকমতো কাজ করছে না, এবং আপনি কারণ বুঝতে পারছেন না। এখানে কীভাবে জাভা ডেভেলপমেন্টের জন্য জনপ্রিয় IDE একটি, ইক্লিপ্স ব্যবহার করে একটি ডিবাগার চালু করবেন:

প্রথমে, নিশ্চিত করুন যে আপনি একটি ব্রেকপয়েন্ট সেট করেছেন। তারপর, ফাইলের উপর ডান ক্লিক করুন, 'Debug As' নির্বাচন করুন, এবং 'Java Application' এ ক্লিক করুন।

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // এখানে একটি ব্রেকপয়েন্ট সেট করুন
        int result = divide(a, b);
        System.out.println("ফলাফল হল: " + result);
    }

    private static int divide(int numerator, int denominator) {
        // একটি ভালো জায়গা একটি ব্রেকপয়েন্টের জন্য
        return numerator / denominator;
    }
}
```

এটি করে, আপনার প্রোগ্রাম ব্রেকপয়েন্টে থেমে যাবে, এবং আপনি ভেরিয়েবল পরীক্ষা করতে, কোডে লাইন ধরে ধরে এগোতে এবং আপনার প্রোগ্রাম কীভাবে আচরণ করে তা দেখতে পারবেন।

ডিবাগার কনসোলে নমুনা আউটপুট:
```
ব্রেকপয়েন্টে হিট হয়েছে লাইনে: int result = divide(a, b);
```

## গভীর ডুব
ডিবাগিং ধারণা প্রোগ্রামিংয়ের প্রাথমিক দিন থেকে চলে আসছে। কিংবদন্তি আছে যে, "বাগ" শব্দটি আসলে কম্পিউটারের ভেতরে একটি মথ-যুক্ত বাগ দ্বারা আবিষ্কৃত হয়েছিল গ্রেস হপার দ্বারা, যিনি এই ক্ষেত্রের একজন অগ্রদূত। আজকের দিনে এসে, আমাদের কাছে IntelliJ IDEA, Eclipse, এবং NetBeans এর মতো জটিল IDE রয়েছে যেগুলি শক্তিশালী ডিবাগার সরবরাহ করে।

IDE ডিবাগার ছাড়া বিকল্পগুলি লগিং, প্রিন্ট স্টেটমেন্ট (গরীবের ডিবাগার), অ্যাসারশন, এবং জেডিবি (Java Debugger) এর মতো স্ট্যান্ডঅ্লোন ডিবাগিং টুলের মতো বিকল্প, যা জাভা ডেভেলপমেন্ট কিটের (JDK) অংশ।

একটি ডিবাগার প্রোগ্রামারকে নির্বাহন মুহূর্তে থামাতে (ব্রেকপয়েন্ট), কোড ধাপে ধাপে যেতে, ভেরিয়েবলের মান পরীক্ষা করতে, সেই মানগুলি চলাকালীন সময়ে পরিবর্তন করতে, এবং এমনকি কোড ব্লকে ব্লকে রান করতে অনুমতি দেয়। একটি ডিবাগারের ব্যবহার প্রায়ই জটিল অ্যাপ্লিকেশন বিকাশে একটি অমূল্য কৌশল হিসেবে বিবেচিত হয় যেখানে একটি সমস্যা সৃষ্টিকারী সঠিক কোডের লাইনটি খুঁজে পাওয়াকে গোল্লাছুটের সূঈ খোঁজা বলা যেতে পারে।

## দেখার জন্য
- ডিবাগিং সম্পর্কে অফিসিয়াল অরাকল ডকুমেন্টেশন: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- ইক্লিপ্সের ডিবাগিং গাইড: [Eclipse Debugging Tips](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- VisualVM, কমান্ড-লাইন JDK টুলগুলি একত্রিত করে একটি ভিজ্যুয়াল টুল এবং লাইটওয়েট প্রোফাইলিং ক্ষমতা: [VisualVM](https://visualvm.github.io/)
