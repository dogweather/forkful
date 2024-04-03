---
date: 2024-01-20 18:00:37.440591-07:00
description: "How to: (\u65B9\u6CD5) TypeScript\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u9001\u308B\u306B\u306F\u3001`fetch` API\u3092\u4F7F\u3044\u307E\u3059\u3002\
  \u4E0B\u8A18\u306E\u30B3\u30FC\u30C9\u3092\u898B\u3066\u304F\u3060\u3055\u3044."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.752653-06:00'
model: gpt-4-1106-preview
summary: "TypeScript\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u306B\
  \u306F\u3001`fetch` API\u3092\u4F7F\u3044\u307E\u3059\u3002\u4E0B\u8A18\u306E\u30B3\
  \u30FC\u30C9\u3092\u898B\u3066\u304F\u3060\u3055\u3044."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
