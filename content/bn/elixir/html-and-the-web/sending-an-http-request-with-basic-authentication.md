---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:06.957203-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Elixir-\u098F \u09AE\u09CC\u09B2\
  \u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\
  \u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF\
  , \u0986\u09AA\u09A8\u09BF `HTTPoison` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\
  \u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09C7\u09A8."
lastmod: '2024-03-17T18:47:43.669025-06:00'
model: gpt-4-0125-preview
summary: "Elixir-\u098F \u09AE\u09CC\u09B2\u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\
  \u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\
  \u099F\u09BF HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09AA\u09BE\u09A0\u09BE\u09A8\
  \u09CB\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BF `HTTPoison` \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
Elixir-এ মৌলিক প্রমাণীকরণের সাথে একটি HTTP অনুরোধ পাঠানোর জন্য, আপনি `HTTPoison` লাইব্রেরি ব্যবহার করতে পারেন:

```elixir
# mix.exs-এ আপনার প্রকল্পের নির্ভরতাগুলিতে HTTPoison যোগ করুন
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# এখন আসুন মৌলিক auth সহ একটি অনুরোধ করি

# নির্ভরতাটি আনতে `mix deps.get` চালান

# আপনার Elixir কোডে
defmodule BasicAuthRequest do
  def send_request do
    # "username:password" ক্রেডেনশিয়ালগুলিকে Base64 ব্যবহার করে এনকোড করুন
    basic_auth =
      "username:password"
      |> Base.encode64()

    # Authorization হেডার সেট করুন
    headers = [{"Authorization", "Basic #{basic_auth}"}]

    # https://example.com/protected-resource এ একটি GET অনুরোধ করুন
    HTTPoison.get("https://example.com/protected-resource", headers)
  end
end

# ফাংশনটি কল করুন
{:ok, response} = BasicAuthRequest.send_request()
IO.inspect(response.status_code)  # যদি প্রমাণীকরণ সফল হয় তবে 200 হওয়া উচিত
```

যদি মৌলিক প্রমাণীকরণ সফল হয়, আপনি একটি `200` স্ট্যাটাস কোড পাবেন। ব্যর্থ প্রমাণীকরণ সাধারণত একটি `401` ফলাফল দেয়।

## গভীর ডুব
মৌলিক প্রমাণীকরণ হল HTTP-এর একটি অংশ যা RFC 7617-এ সংজ্ঞায়িত, এটি খুব প্রাথমিক ওয়েব থেকে ডেট করা হয়েছে। এটি সহজ কিন্তু আধুনিক পদ্ধতিগুলির তুলনায় কম নিরাপদ, প্রতিটি অনুরোধে ক্রেডেনশিয়াল পাঠানো হয় (বেস64 এনকোডেড তবে এনক্রিপ্টেড নয়)।

বিকল্পগুলো অন্তর্ভুক্ত:
- OAuth: অ্যাক্সেস প্রতিনিধিত্বের জন্য একটি খোলা মান, যা অন্যান্য ওয়েবসাইটগুলিতে তাদের পাসওয়ার্ড ছাড়াই ওয়েবসাইট বা অ্যাপ্লিকেশনগুলিকে তাদের তথ্যে অ্যাক্সেস দেওয়ার জন্য ব্যবহৃত হয়।
- API কী: API অনুরোধে একটি ব্যবহারকারী বা একটি প্রোগ্রামকে প্রমাণীকরণের জন্য ব্যবহৃত অনন্য পরিচয়কারী।
- বিয়ারার টোকেন/JWT: নিরাপত্তা টোকেনগুলি যা ব্যবহারকারীকে প্রমাণীকরণের জন্য প্রয়োজনীয় সমস্ত ব্যবহারকারী ডেটা ধারণ করে।

বাস্তবায়নের দিক থেকে, `HTTPoison` ব্যবহার করে আমরা:
- ব্যবহারকারী নাম এবং পাসওয়ার্ডকে Base64 এনকোড করি;
- "Basic " দ্বারা পূর্বসূরী করা এই এনকোডেড স্ট্রিংটি `Authorization` হেডারে অন্তর্ভুক্ত করি;
- সার্ভারে অনুরোধ পাঠাই আশায় অ্যাক্সেস পাওয়া যাবে।

মনে রাখবেন, মৌলিক auth স্পষ্ট পাঠ্য এবং সহজেই ডিকোড করা যায়। এটি শুধুমাত্র HTTPS এর উপরে নিরাপদ।

## দেখুন এছাড়াও
- HTTPoison ডকুমেন্টেশন: https://hexdocs.pm/httpoison
- মৌলিক প্রমাণীকরণ স্কিমা (RFC 7617): https://tools.ietf.org/html/rfc7617
- Elixir-এর `Base` মডিউল ডকুমেন্টেশন: https://hexdocs.pm/elixir/Base.html
- OAuth 2.0 অনুমোদন ফ্রেমওয়ার্ক: https://oauth.net/2/
