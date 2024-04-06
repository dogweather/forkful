---
date: 2024-01-26 01:09:45.059725-07:00
description: "\u65B9\u6CD5\uFF1A Bash\u306B\u304A\u3044\u3066\u3001\u95A2\u6570\u306F\
  \u521D\u671F\u30D0\u30FC\u30B8\u30E7\u30F3\u304B\u3089\u30B3\u30FC\u30C9\u3092\u533A\
  \u753B\u5316\u3059\u308B\u624B\u6BB5\u3068\u3057\u3066\u3042\u308A\u307E\u3057\u305F\
  \u3002\u6B74\u53F2\u7684\u306B\u8A00\u3048\u3070\u3001\u95A2\u6570\u3092\u4F7F\u7528\
  \u3059\u308B\u3053\u3068\u306F\u3001\u30B3\u30FC\u30C9\u54C1\u8CEA\u3092\u5411\u4E0A\
  \u3055\u305B\u308B\u305F\u3081\u306B1960\u5E74\u4EE3\u306B\u5C0E\u5165\u3055\u308C\
  \u305F\u69CB\u9020\u5316\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u539F\u5247\
  \u306B\u6CBF\u3063\u3066\u3044\u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:50:56.279817-06:00'
model: gpt-4-1106-preview
summary: "\u95A2\u6570\u306E\u4EE3\u66FF\u624B\u6BB5\u306B\u306F\u3001\u30B9\u30AF\
  \u30EA\u30D7\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u30BD\u30FC\u30B7\u30F3\u30B0\u3057\
  \u305F\u308A\u30A8\u30A4\u30EA\u30A2\u30B9\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u304C\u3042\u308A\u307E\u3059\u304C\u3001\u3053\u308C\u3089\u306F\u540C\u3058\u30EC\
  \u30D9\u30EB\u306E\u30E2\u30B8\u30E5\u30FC\u30EB\u6027\u3084\u518D\u5229\u7528\u6027\
  \u3092\u63D0\u4F9B\u3057\u307E\u305B\u3093\u3002"
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法：
Bashでシンプルな関数を作成します：

```Bash
greet() {
  echo "Hello, $1!"
}
```

パラメーターを指定して関数を呼び出すには：

```Bash
greet "World"  # 出力：Hello, World!
```

関数は`return`を使用して数値ステータスコードを返します（実際のデータ返却には使用しません）：

```Bash
add() {
  return $(($1 + $2))
}

add 3 4
echo $?  # 出力：7
```

`$?`は最後のコマンドの戻り値をキャプチャすることに注意してください。これは`add`の数値結果です。

## 詳細解説
Bashにおいて、関数は初期バージョンからコードを区画化する手段としてありました。歴史的に言えば、関数を使用することは、コード品質を向上させるために1960年代に導入された構造化プログラミングの原則に沿っています。

関数の代替手段には、スクリプトファイルをソーシングしたりエイリアスを使用することがありますが、これらは同じレベルのモジュール性や再利用性を提供しません。

Bashにおける注目すべき実装の詳細は、関数は第一級の市民であるということです；他の言語のような特定の宣言キーワード「function」はありませんが、可読性のためにBashでは「function」がオプショナルです。関数のスコープも興味深いものがあります—変数はデフォルトでグローバルであるため、適切に管理されない場合は予期せぬ動作につながることがあります。

## 参照
- Bashマニュアルのシェル関数: https://www.gnu.org/software/bash/manual/html_node/Shell-Functions.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/functions.html
- 「Pro Bash Programming: Scripting the GNU/Linux Shell」で、関数スクリプティングのコンセプトと実践に関する深い知識を得る。
