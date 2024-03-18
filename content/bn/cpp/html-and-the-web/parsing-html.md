---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:07.587602-06:00
description: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 HTML \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\u0995\u09C7\
  \ \u098F\u09AE\u09A8 \u0995\u09BF\u099B\u09C1\u09A4\u09C7 \u09AD\u09C7\u0999\u09C7\
  \ \u09AB\u09C7\u09B2\u09BE \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09AC\u09C1\u099D\u09A4\u09C7 \u098F\u09AC\
  \u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE \u09A8\
  \u09BF\u09B7\u09CD\u0995\u09BE\u09B6\u09A8, \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\
  \u09B8\u09CD\u09A4\u09C1\u2026"
lastmod: '2024-03-17T18:47:44.363080-06:00'
model: gpt-4-0125-preview
summary: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 HTML \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\u09B8\u09CD\u09A4\u09C1\u0995\u09C7\
  \ \u098F\u09AE\u09A8 \u0995\u09BF\u099B\u09C1\u09A4\u09C7 \u09AD\u09C7\u0999\u09C7\
  \ \u09AB\u09C7\u09B2\u09BE \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE \u09AC\u09C1\u099D\u09A4\u09C7 \u098F\u09AC\
  \u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A1\u09C7\u099F\u09BE \u09A8\
  \u09BF\u09B7\u09CD\u0995\u09BE\u09B6\u09A8, \u09AC\u09BF\u09B7\u09AF\u09BC\u09AC\
  \u09B8\u09CD\u09A4\u09C1\u2026"
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
HTML পার্স করা মানে HTML বিষয়বস্তুকে এমন কিছুতে ভেঙে ফেলা যা একটি প্রোগ্রাম বুঝতে এবং ম্যানিপুলেট করতে পারে। প্রোগ্রামাররা ডেটা নিষ্কাশন, বিষয়বস্তু ম্যানিপুলেট করা, বা তাদের অ্যাপ্লিকেশনে ওয়েব স্ক্র্যাপিং ইন্টিগ্রেট করার জন্য এটি করে।

## কিভাবে:
C++ নিজে থেকে HTML পার্সিং সুবিধা সহ আসে না। আপনি সাধারণত Google এর Gumbo-parser বা অনুরূপ কোনো লাইব্রেরি ব্যবহার করবেন। এখানে Gumbo-parser ব্যবহার করে একটি দ্রুত উদাহরণ দেওয়া হল:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

নমুনা আউটপুট:
```
https://example.com
```

## গভীর ডাইভ
C++ এ HTML পার্স করা সবসময় সরল পথ ছিল না। ঐতিহাসিকভাবে, প্রোগ্রামাররা রেগেক্স বা হাতে লেখা পার্সার ব্যবহার করতো, উভয়ই ত্রুটিপূর্ণ এবং জটিল ছিল। বর্তমান সময়ে, Gumbo-parser এর মতো দৃঢ় লাইব্রেরিগুলি পার্সিংয়ের জটিলতাগুলি সামলান, এটি সহজ এবং আরও বিশ্বস্ত করে তোলে।

বিকল্পগুলি হিসাবে Tidy, MyHTML অথবা C++ কে Python এর BeautifulSoup এর সাথে C++ `system` ফাংশন বা এম্বেডেড ইন্টারপ্রিটার ব্যবহার করে ইন্টিগ্রেট করা রয়েছে।

বাস্তবায়নের দিক থেকে, এই লাইব্রেরিগুলি HTML কে একটি ডকুমেন্ট অবজেক্ট মডেল (DOM) গাছে রূপান্তর করে। DOM ট্রাভার্স এবং ম্যানিপুলেশন ব্যবহারকারীদের ডেটা নিষ্কাশন এবং কাজ করার জন্য দেখায়, যেমনটি কিভাবে বিভাগে দেখানো হয়েছে।

## আরও দেখুন
- [Gumbo-parser GitHub রিপোজিটরি](https://github.com/google/gumbo-parser)
- [HTML পার্সিং লাইব্রেরির তালিকা](https://en.cppreference.com/w/c/experimental/dynamic)
- [C++ এবং Python মাধ্যমিকতা](https://docs.python.org/3/extending/embedding.html)
