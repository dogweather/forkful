---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:37.440591-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

category:             "TypeScript"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request.md"
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
