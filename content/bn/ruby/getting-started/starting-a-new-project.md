---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:21:35.255419-06:00
description: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA\
  \ \u0986\u09B0\u09AE\u09CD\u09AD \u0995\u09B0\u09BE \u0986\u09AA\u09A8\u09BE\u09B0\
  \ \u09A1\u09BF\u099C\u09BF\u099F\u09BE\u09B2 \u09AC\u09BE\u0997\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09AC\u09C0\u099C \u09AC\u09AA\u09A8\u09C7\u09B0 \u09AE\
  \u09A4\u2014\u0986\u09AA\u09A8\u09BF \u098F\u0995 \u0997\u09C1\u099A\u09CD\u099B\
  \ \u09A8\u09A4\u09C1\u09A8 \u099A\u09BF\u09A8\u09CD\u09A4\u09BE \u09B6\u09C1\u09B0\
  \u09C1 \u0995\u09B0\u099B\u09C7\u09A8 \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\
  \u09B0\u0995\u09C7 \u098F\u09AE\u09A8 \u0995\u09CB\u09A1\u09C7 \u09B0\u09C2\u09AA\
  \u09BE\u09A8\u09CD\u09A4\u09B0\u09BF\u09A4 \u0995\u09B0\u099B\u09C7\u09A8 \u09AF\
  \u09BE \u0995\u09BF\u099B\u09C1 \u0989\u09AA\u0995\u09BE\u09B0\u09C0\u2026"
lastmod: '2024-03-17T18:47:44.591006-06:00'
model: gpt-4-0125-preview
summary: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u0986\
  \u09B0\u09AE\u09CD\u09AD \u0995\u09B0\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09A1\
  \u09BF\u099C\u09BF\u099F\u09BE\u09B2 \u09AC\u09BE\u0997\u09BE\u09A8\u09C7 \u098F\
  \u0995\u099F\u09BF \u09AC\u09C0\u099C \u09AC\u09AA\u09A8\u09C7\u09B0 \u09AE\u09A4\
  \u2014\u0986\u09AA\u09A8\u09BF \u098F\u0995 \u0997\u09C1\u099A\u09CD\u099B \u09A8\
  \u09A4\u09C1\u09A8 \u099A\u09BF\u09A8\u09CD\u09A4\u09BE \u09B6\u09C1\u09B0\u09C1\
  \ \u0995\u09B0\u099B\u09C7\u09A8 \u098F\u09AC\u0982 \u09A4\u09BE\u09A6\u09C7\u09B0\
  \u0995\u09C7 \u098F\u09AE\u09A8 \u0995\u09CB\u09A1\u09C7 \u09B0\u09C2\u09AA\u09BE\
  \u09A8\u09CD\u09A4\u09B0\u09BF\u09A4 \u0995\u09B0\u099B\u09C7\u09A8 \u09AF\u09BE\
  \ \u0995\u09BF\u099B\u09C1 \u0989\u09AA\u0995\u09BE\u09B0\u09C0\u2026"
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

নতুন প্রকল্প আরম্ভ করা আপনার ডিজিটাল বাগানে একটি বীজ বপনের মত—আপনি এক গুচ্ছ নতুন চিন্তা শুরু করছেন এবং তাদেরকে এমন কোডে রূপান্তরিত করছেন যা কিছু উপকারী কাজ করে। প্রোগ্রামাররা সমস্যা সমাধানের জন্য, ধারণাগুলি অন্বেষণ করার জন্য, অথবা কেবল নতুন কিছু সৃষ্টির আনন্দের জন্য নতুন প্রকল্প শুরু করে।

## কিভাবে:

তাহলে, আপনি আপনার মস্তিষ্কের তরঙ্গগুলিকে একটি Ruby প্রকল্পে পরিণত করার জন্য প্রস্তুত? চলুন শুরু করা যাক। মৌলিক বিষয় দিয়ে শুরু।

```Ruby
# Ruby ইনস্টল করুন, যদি ইতিমধ্যে না থাকে।
# আপনার Ruby সংস্করণ চেক করুন নিশ্চিত হতে যে আপনি আপডেটের সাথে আছেন:
$ ruby -v

# আউটপুট Ruby এর বর্তমান সংস্করণ হওয়া উচিত:
# ruby 3.x.x

# এরপর, আপনার প্রকল্পের জন্য একটি ডিরেক্টরী তৈরি করা যাক:
$ mkdir my_new_project
$ cd my_new_project

# এখন, ভার্সন কন্ট্রোলের জন্য একটি নতুন Git রিপোজিটরি শুরু করুন (খুবই সুপারিশযোগ্য):
$ git init

# তারপর একটি এন্ট্রি ফাইল তৈরি করুন, আমরা এটিকে 'app.rb' বলি:
$ touch app.rb

# কোডিং শুরু করুন! এটি কাজ করছে কিনা নিশ্চিত করতে একটি সহজ আউটপুট লিখুন:
puts "Hello New Project!"

# আপনার ফাইল চালান:
$ ruby app.rb

# আউটপুট হওয়া উচিত:
# Hello New Project!
```

## গভীর ডুব

পূর্বে, একটি নতুন Ruby প্রকল্প শুরু করা একটু কাঁচা ছিল—কেবল আপনি, একটি টেক্সট এডিটর, এবং এক গুচ্ছ `.rb` ফাইল। ভাষাটি বেড়ে ওঠার সাথে সাথে, এই প্রক্রিয়াটি সহজীকরণের জন্য টূলগুলি প্রকাশ পেয়েছে।

উদাহরণস্বরূপ, Bundler আপনার গেমস—Ruby লাইব্রেরিগুলি—পরিচালনা করে, যাতে আপনি সহজে নির্ভরতা ট্র্যাক করতে ও ইনস্টল করতে পারেন। কেবল `bundle init` চালান আপনার প্রকল্পের ডিরেক্টরি স্থাপনার পর একটি `Gemfile` তৈরি করে, যেখানে আপনার গেমগুলিকে তালিকাভুক্ত করবেন।

তারপর আমাদের আছে Ruby Version Manager (RVM) এবং Ruby Environment (rbenv), যা প্রকল্প অনুযায়ী Ruby সংস্করণ পরিবর্তন করতে সহায়তা করে। পুরানো কোড নিয়ে জাগলিং করলে বেশ সুবিধাজনক।

এবং ফ্রেমওয়ার্কগুলো সম্পর্কে কি? Ruby on Rails ওয়েব অ্যাপসের জন্য বড় নাম। তবে আপনি যদি হালকা ওজনের (যেমন সার্ভিস অথবা APIs এর জন্য) কিছু চান, তাহলে Sinatra অথবা Roda দেখুন।

## দেখুন এছাড়াও

- Ruby এর অফিসিয়াল সাইট আপডেট এবং ডকুমেন্টেশনের জন্য: [https://www.ruby-lang.org](https://www.ruby-lang.org)
- Bundler, আপনার Ruby গেম পরিচালনার জন্য: [https://bundler.io](https://bundler.io)
- RVM, এক Ruby Version Manager: [https://rvm.io](https://rvm.io)
- rbenv, আপনার প্রকল্পের জন্য Ruby সংস্করণ নির্বাচনের জন্য: [https://github.com/rbenv/rbenv](https://github.com/rbenv/rbenv)
- Sinatra, একটি হালকা ওজনের ওয়েব ফ্রেমওয়ার্ক: [http://sinatrarb.com](http://sinatrarb.com)
- কোড শেয়ারিং এবং সহযোগিতার জন্য, GitHub আপনার সেরা বন্ধু: [https://github.com](https://github.com)