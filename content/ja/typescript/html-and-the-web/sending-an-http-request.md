---
aliases:
- /ja/typescript/sending-an-http-request/
date: 2024-01-20 18:00:37.440591-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\
  \u3046\u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\
  \u3059\u308B\u305F\u3081\u306E\u624B\u6BB5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30A6\
  \u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u306B\u5BFE\u3057\u3066\u52D5\u4F5C\u3092\u8D77\
  \u3053\u3059\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3046\u3093\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.678996
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3044\u3046\
  \u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\u5831\u3092\u8981\u6C42\u3059\
  \u308B\u305F\u3081\u306E\u624B\u6BB5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30A6\u30A7\
  \u30D6\u30B5\u30FC\u30D3\u30B9\u306B\u5BFE\u3057\u3066\u52D5\u4F5C\u3092\u8D77\u3053\
  \u3059\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3046\u3093\u3067\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTTPリクエストを送るっていうのは、サーバーに情報を要求するための手段です。プログラマーはデータを取得したり、ウェブサービスに対して動作を起こすためにこれを行うんです。

## How to: (方法)

TypeScriptでHTTPリクエストを送るには、`fetch` APIを使います。下記のコードを見てください:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fetching data error:', error);
  }
}

fetchData('https://api.example.com/data');
```

サンプル出力:

```json
{
  "userId": 1,
  "id": 1,
  "title": "Sample Data",
  "completed": false
}
```

## Deep Dive (深掘り)

HTTPリクエストの送信はWebの始まりから重要な部分でした。`XMLHttpRequest` が古典的な方法でしたが、`fetch` APIはよりモダンな代替手段です。

`fetch` APIはプロミスを使うので、非同期コードを書きやすくしています。古くからある `XMLHttpRequest` と比べ、シンプルで読みやすいコードが書けます。もし`fetch`にまだ慣れていなければ、`Axios`などのライブラリを使うという選択肢もありますが、ネイティブな`fetch` APIは外部ライブラリに頼らない力強い選択肢です。

実装の詳細に入ると、HTTPリクエストの種類(GET, POST, PUT, DELETEなど)、ステータスコード(200は成功、404は見つからない、等)、ヘッダーやボディの管理など、把握すべきことが多いです。

## See Also (関連するリソース)

- MDN Web Docs on Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/intro.html
- Using Axios with TypeScript: https://github.com/axios/axios
