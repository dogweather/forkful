---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:23.938471-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: C \u09AD\u09BE\u09B7\u09BE\u09AF\
  \u09BC, \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\
  \u09B2\u09BF \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09A6\u09CD\u09AC\
  \u09BE\u09B0\u09BE \u09B8\u09AE\u09B0\u09CD\u09A5\u09BF\u09A4, \u09AC\u09BF\u09B6\
  \u09C7\u09B7\u09A4 `<complex.h>`\u0964 \u098F\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7, `double complex` \u099F\
  \u09BE\u0987\u09AA\u09C7\u09B0 (\u0985\u09A5\u09AC\u09BE \u098F\u0995\u0995\u2026"
lastmod: '2024-03-17T18:47:44.535080-06:00'
model: gpt-4-0125-preview
summary: "C \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC, \u099C\u099F\u09BF\u09B2 \u09B8\u0982\
  \u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\
  \u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u09B8\u09AE\u09B0\u09CD\u09A5\
  \u09BF\u09A4, \u09AC\u09BF\u09B6\u09C7\u09B7\u09A4 `<complex.h>`\u0964 \u098F\u0997\
  \u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\
  \u09C7, `double complex` \u099F\u09BE\u0987\u09AA\u09C7\u09B0 (\u0985\u09A5\u09AC\
  \u09BE \u098F\u0995\u0995 \u09A8\u09BF\u09B0\u09CD\u09AD\u09C1\u09B2\u09A4\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF `float complex`) \u09B8\u09BE\u09A5\u09C7 \u09AD\
  \u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u0998\u09CB\u09B7\u09A3\u09BE\
  \ \u0995\u09B0\u09C1\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09AC\u09C7\u09B8\
  \u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\
  \ \u0995\u09B0\u09BE\u09B0 \u0989\u09AA\u09BE\u09AF\u09BC \u09A6\u09C7\u0996\u09BE\
  \u09A8\u09CB \u09B9\u09B2\u09CB."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
C ভাষায়, জটিল সংখ্যাগুলি স্ট্যান্ডার্ড লাইব্রেরি দ্বারা সমর্থিত, বিশেষত `<complex.h>`। এগুলি ব্যবহার করতে, `double complex` টাইপের (অথবা একক নির্ভুলতার জন্য `float complex`) সাথে ভেরিয়েবল ঘোষণা করুন। এখানে বেসিক অপারেশনগুলি করার উপায় দেখানো হলো:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // একটি জটিল সংখ্যা 1+2i ঘোষণা করা হল
    double complex z2 = 1.0 - 2.0*I; // আরেকটি জটিল সংখ্যা 1-2i ঘোষণা করা হল
    
    // যোগ
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // আউটপুট: Sum: 2.00 + 0.00i

    // গুণ
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // আউটপুট: Product: 5.00 + 0.00i

    // জটিল সংযোজক
    double complex conjugate = conj(z1);
    printf("Conjugate of z1: %.2f + %.2fi\n", creal(conjugate), cimag(conjugate)); // আউটপুট: Conjugate of z1: 1.00 - 2.00i
    
    // মাত্রা
    double magnitude = cabs(z1);
    printf("Magnitude of z1: %.2f\n", magnitude); // আউটপুট: Magnitude of z1: 2.24

    // পর্যায়
    double phase = carg(z1);
    printf("Phase of z1: %.2f\n", phase); // আউটপুট রেডিয়ান মাত্রিক
    
    return 0;
}
```
`I` হলো `<complex.h>` মধ্যে একটি ধ্রুবক যা কাল্পনিক ইউনিটকে প্রকাশ করে। `creal()` এবং `cimag()` ফাংশনগুলি যথাক্রমে বাস্তব এবং কাল্পনিক অংশগুলি প্রকাশ করে, যখন `conj()` জটিল সংযোজক গণনা করে। জটিল সংখ্যাগুলির মাত্রা এবং পর্যায় (আর্গুমেন্ট) জন্য, `cabs()` এবং `carg()` ব্যবহৃত হয়।

## গভীর ডুব
C ভাষায় জটিল সংখ্যাগুলির জন্য সমর্থন C99 এ স্ট্যান্ডার্ডাইজ হিসেবে অপেক্ষাকৃত সাম্প্রতিক, এর আগে C ভাষায় জটিল সংখ্যা গণনা কঠিন ছিল, প্রায়ই কাস্টম ডেটা স্ট্রাকচার এবং ফাংশন প্রয়োজন হত। `<complex.h>` এবং জটিল ডেটা টাইপগুলির অন্তর্ভুক্তি বিজ্ঞানী এবং ইঞ্জিনিয়ারিং অ্যাপ্লিকেশনের জন্য ভাষার ক্ষমতায় একটি গুরুত্বপূর্ণ উন্নতি প্রদান করে। তবে, আরও প্রাতিষ্ঠানিক সমর্থন এবং একটি সমৃদ্ধতর লাইব্রেরি ফাংশনের সেটের মাধ্যমে জটিল সংখ্যাগুলির জন্য পাইথনের মতো কিছু ভাষা আরও সহজতর সমর্থন প্রদান করে। এরপরেও, উচ্চ-কার্যকারিতা কম্পিউটিং কাজের জন্য C একটি পছন্দের পছন্দ থাকে যদিও এর মানে জটিল অংক গাণিতিকের জন্য সামান্য বেশি বর্ণনামূলক সিনট্যাক্সের সাথে ডিল করা।
