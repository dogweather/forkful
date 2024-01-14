---
title:    "Rust: 「現在の日付の取得」"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ？
Rustプログラミングを始めたばかりの人にとって、日付を取得することは重要なスキルです。そして、その他のプログラミング言語と比べて、Rustは特に日付を取得するのに便利です。今日はRustで現在の日付を取得する方法を詳しくご説明します。

## 方法
まずは、標準ライブラリの一つである`std::time`をインポートしましょう。

```Rust
use std::time;
```

次に、`time::SystemTime`を使って、現在の日付データを取得することができます。

```Rust
let now = time::SystemTime::now();
```

そして、`now`をフォーマットして、任意の形式で日付を出力することができます。

```Rust
println!("今日の日付は{:?}", now);
// 出力: 今日の日付はOk(2021-09-21 12:30:00)
```

また、フォーマットを変更することもできます。例えば、`strftime`を使ってフォーマットを指定することができます。

```Rust
println!("今日の日付は{}", now.strftime("%Y年%m月%d日").unwrap());
// 出力: 今日の日付は2021年09月21日
```

## ディープダイブ
さらに、`SystemTime`には、現在の日付と比較するメソッドもあります。例えば、`elapsed`メソッドを使うと、現在の日付からどれだけ時間が経過したかを取得することができます。

```Rust
let prev_time = time::SystemTime::now();
// 1秒待機
std::thread::sleep(std::time::Duration::from_secs(1));
let elapsed = prev_time.elapsed().unwrap();
println!("1秒経過しましたか？: {}", now > elapsed);
// 出力: 1秒経過しましたか？: true
```

`elapsed`メソッド以外にも、様々なメソッドがありますので、ぜひ参考にしてみてください。

## おわりに
今日は、Rustで現在の日付を取得する方法をご紹介しました。日付の取得だけでなく、現在の日付からどれだけ時間が経過したかを計算することもできます。Rustなら、簡単に日付を取得することができるので、ぜひ試してみてください！

## 関連リンク
- [Rust標準ライブラリの日付取得のドキュメント](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Rustで日付をフォーマットする方法](https://stackoverflow.com/questions/47425190/format-timestamp-in-rust-language)