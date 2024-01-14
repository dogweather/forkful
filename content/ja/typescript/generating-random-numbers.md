---
title:                "TypeScript: ランダムな数の生成"
simple_title:         "ランダムな数の生成"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数を生成することのメリットについて説明します。テストやゲーム、シミュレーションなど、多くのプログラムでランダムな数が必要になります。この記事では、TypeScriptを使用してランダムな数を生成する方法を紹介します。

## 方法

まず、ランダムな数を生成するためには、TypeScriptの `Math` ライブラリを使用します。以下のコードは、1から10までのランダムな数を生成する例です。

```TypeScript
let randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum);
```

出力結果は以下のようになります。

```
5  // 1から10までの数のうち、どれか1つがランダムに出力される
```

また、配列からランダムに要素を選択することもできます。次のコードでは、数字の配列からランダムに1つの数字を選択しています。

```TypeScript
let numbers = [1, 2, 3, 4, 5];
let randomNum = numbers[Math.floor(Math.random() * numbers.length)];
console.log(randomNum);
```

出力結果は以下のようになります。

```
3  // 配列の要素の中から、ランダムに1つが選択される 
```

複数のランダムな数を生成するには、ループを使うことができます。

```TypeScript
for (let i = 0; i < 5; i++) {
  let randomNum = Math.floor(Math.random() * 100) + 1;
  console.log(randomNum);
}
```

出力結果は以下のようになります。

```
65
23
89
42
7
```

## ディープダイブ

TypeScriptの `Math` ライブラリを使用してランダムな数を生成する方法については、以上で説明しました。しかし、ランダムな数の生成には様々なアルゴリズムや技術があります。ランダム性の高い数を生成するには、より複雑な方法やツールを使用する必要があります。この記事では紹介しきれませんが、興味のある方はさらに深く調べてみてください。

## 関連リンク

- [TypeScript公式ドキュメント](https://www.typescriptlang.org/)
- [ランダム数を生成する方法](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random) (英語)
- [より複雑なランダム数を生成する方法](https://www.digitalocean.com/community/tutorials/how-to-generate-random-numbers-in-javascript) (英語)

お疲れ様でした。ランダム数を生成する上で役に立つ情報をお届けできたことを願っています。お楽しみください！