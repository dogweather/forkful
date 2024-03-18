---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:35.382672-06:00
description: "Elixir \u098F XML \u09A8\u09BF\u09DF\u09C7 \u0995\u09BE\u099C \u09AE\
  \u09BE\u09A8\u09C7 \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982, \u09A4\u09C8\u09B0\
  \u09BF \u098F\u09AC\u0982 XML \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09DF\u09C7\u09AC\
  \ \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8, \u0995\u09A8\u09AB\u09BF\u0997 \u09AB\
  \u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09B2\u09BF\u0997\u09CD\u09AF\u09BE\u09B8\
  \u09BF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09AC\u09CD\u09AF\
  \u09BE\u09AA\u0995\u09A4\u09BE\u09DF XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.697593-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u098F XML \u09A8\u09BF\u09DF\u09C7 \u0995\u09BE\u099C \u09AE\u09BE\
  \u09A8\u09C7 \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982, \u09A4\u09C8\u09B0\u09BF\
  \ \u098F\u09AC\u0982 XML \u09A1\u09C7\u099F\u09BE \u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0993\u09DF\u09C7\u09AC\
  \ \u09B8\u09BE\u09B0\u09CD\u09AD\u09BF\u09B8, \u0995\u09A8\u09AB\u09BF\u0997 \u09AB\
  \u09BE\u0987\u09B2 \u098F\u09AC\u0982 \u09B2\u09BF\u0997\u09CD\u09AF\u09BE\u09B8\
  \u09BF \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09AC\u09CD\u09AF\
  \u09BE\u09AA\u0995\u09A4\u09BE\u09DF XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\u2026"
title: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
Elixir এ XML নিয়ে কাজ মানে পার্সিং, তৈরি এবং XML ডেটা ম্যানিপুলেট করা। প্রোগ্রামাররা ওয়েব সার্ভিস, কনফিগ ফাইল এবং লিগ্যাসি সিস্টেমের ব্যাপকতায় XML এর সাথে কাজ করে।

## কিভাবে:
Elixir এর স্ট্যান্ডার্ড লাইব্রেরিতে XML পার্সিং অন্তর্ভুক্ত নেই। SweetXML একটি জনপ্রিয় পছন্দ। এটি ব্যবহার করার উপায় হল:

```elixir
# mix.exs এ আপনার ডিপেন্ডেন্সিতে SweetXML যোগ করুন
{:sweet_xml, "~> 0.6"}

# আপনার কোডে
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# XML পার্স করুন
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # আউটপুট: Tove
```

## গভীরে ডুব দেওয়া
XML, বা Extensible Markup Language, ৯০ এর দশকের শেষ নাগাদ প্রচলনে এসেছে। এটি বাচনবহুল কিন্তু গঠনমূলক—জটিল ডেটা ইন্টারচেঞ্জের জন্য আদর্শ। JSON এর জনপ্রিয়তা এর সাদাসিদে কাঠামোর জন্য আকাশচুম্বী, তবু XML বহু এন্টারপ্রাইজ এবং ফাইন্যান্সিয়াল সিস্টেমে তার প্রকাশনামূলকতা এবং মানকৃত স্কিমার জন্য গভীরভাবে প্রতিষ্ঠিত।

বিকল্প অন্তর্ভুক্ত:
- JSON, কম বাচনবহুল ডেটা বিনিময়ের জন্য।
- Protobuf বা Thrift, বাইনারি সিরিয়ালাইজড ডেটা যোগাযোগের জন্য, বিশেষ করে অভ্যন্তরীণ সিস্টেমের জন্য।

অভ্যন্তরীণভাবে, Elixir এর XML লাইব্রেরিগুলি পার্সিংয়ের জন্য Erlang এর :xmerl লাইব্রেরি ব্যবহার করে, যা দৃঢ় সমর্থন প্রদান করে কিন্তু আধুনিক পদ্ধতি থেকে কম স্বজ্ঞাত হতে পারে। Elixir বিকশিত হওয়ার সাথে সাথে, সামগ্রিক সম্প্রদায়-নিয়ন্ত্রিত লাইব্রেরিসুলি যেমন SweetXML, এগুলিকে আরও Elixir-এসক সিনট্যাক্সের সাথে মোড়ানো হয়, যা XML ম্যানিপুলেশনগুলিকে আরও প্রাপ্য করে তোলে।

## আরও দেখুন:
- SweetXML Hex এ: https://hex.pm/packages/sweet_xml
- Elixir এর XML পার্সিং সম্পর্কে: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- অভ্যন্তরীণ XML হ্যান্ডলিংয়ের জন্য xmerl ডকুমেন্টেশন: http://erlang.org/doc/apps/xmerl/index.html
