---
title:    "Haskell: 現在の日付を取得する"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

Haskellで現在の日付を取得するのに役立つ理由はたくさんあります。なぜなら、現実世界のアプリケーションでは、現在の日付を使用することがよくあるからです。例えば、日付が重要なイベントを追跡するアプリケーションや、締め切りを計算するアプリケーションなどがあります。

## 方法

Haskellで現在の日付を取得するのはとても簡単です。まず、`Data.Time`モジュールをインポートします。

```Haskell
import Data.Time
```

次に、`getCurrentTime`関数を使用して現在の日付を取得します。

```Haskell
currentDate <- getCurrentTime
```

最後に、`formatTime`関数を使用して、取得した日付を指定した形式にフォーマットします。

```Haskell
let formattedDate = formatTime defaultTimeLocale "%Y年%m月%d日" currentDate
```

このようにすると、`formattedDate`には現在の日付が"2021年04月15日"のような形式で格納されます。

## 深堀り

Haskellには、時間の計算やタイムゾーンの処理など、さまざまな機能があるため、現在の日付を取得する方法は多岐にわたります。また、Haskellの型システムを活用して、日付をより安全に扱うこともできます。

## 関連リンク

- [Haskell公式ドキュメント](https://www.haskell.org/)
- [Haskellチュートリアル](https://wiki.haskell.org/Tutorials)
- [Data.Timeモジュールのドキュメント](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskellで日付を扱う方法](https://qiita.com/kazu69/items/065b253efa313fb095c3)