---
title:                "Clojure: コンピュータプログラミングのための「コマンドライン引数の読み込み」"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

＃＃なぜ
コマンドライン引数を読み込むことについて学ぶのは重要です。プログラムをより動的に制御したり、ユーザーの入力に基づいて処理を行ったりするために必要です。

＃＃方法
コマンドライン引数を読み込むには、`main`関数内で`args`パラメーターを使用します。例えば、以下のように入力値を出力するプログラムを作成することができます。

`` `Clojure
(defn -main [& args]
  (println "入力値は：" args))
```

入力値が`java -jar myprogram.jar input1 input2`の場合、出力は`入力値は： (input1 input2)`となります。

＃＃深い潜り込み
コマンドライン引数を読み込むことで、プログラムをより動的に制御できます。`getopts`関数を使用すると、引数をオプションとして受け取ることができます。また、`clojure.tools.cli`ライブラリを使用すると、より多くのオプションや引数を処理できるようになります。詳細については、公式ドキュメントを参照してください。

＃＃参考リンク
- 公式ドキュメント：https://clojure.org/reference/java_command_line_tools
- `getopts`関数の使用方法：https://stackoverflow.com/questions/47492213/clojure-command-line-arguments
- `clojure.tools.cli`ライブラリの使用方法：https://github.com/clojure/tools.cli