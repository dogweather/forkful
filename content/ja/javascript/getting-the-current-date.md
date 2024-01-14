---
title:    "Javascript: 「現在の日付を取得する」"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

＃＃ なぜ - 現在の日付を取得する理由

プログラミングをするとき、時には現在の日付が必要になることがあります。例えば、ブログ記事やソーシャルメディア投稿のタイムスタンプを表示する必要がある場合などです。ただし、毎回手動で日付を入力するのではなく、自動的に現在の日付を取得できる方法があります。今回は、Javascriptを使用して現在の日付を取得する方法を紹介します。

＃＃＃ 方法

まず、Dateオブジェクトを使って現在の時刻を取得します。これは以下のように書くことができます。

```Javascript
let currentDate = new Date();
```

これで変数currentDateに現在の日付が格納されました。次に、その日付を任意の形式で表示することができます。例えば、日付を「月/日/年」の形式で表示したい場合は、以下のように書きます。

```Javascript
let month = currentDate.getMonth() + 1; // 月は0-11で表されるため、+ 1をする
let day = currentDate.getDate();
let year = currentDate.getFullYear();

console.log(month + "/" + day + "/" + year); // 出力: 7/13/2021 (本日の日付を取得した場合)
```

＃＃＃ ディープダイブ

Dateオブジェクトには、さまざまなメソッドが用意されています。例えば、getMonth()のように特定の部分の値を取得するだけではなく、setMonth()のように値を設定することもできます。また、これらのメソッドにはオプションの引数を渡すこともできます。詳細は公式ドキュメントを参照してください。

＃＃＃＃ See Also

- [Dateオブジェクトの公式ドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [日付と時刻を扱う - MDN Web Docs](https://developer.mozilla.org/ja/docs/Learn/JavaScript/First_steps/What_is_JavaScript#%E6%97%A5%E4%BB%98%E3%81%A8%E6%99%82%E5%88%BB%E3%82%92%E6%89%B1%E3%81%86)