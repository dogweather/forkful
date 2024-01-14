---
title:    "Ruby: 读取文本文件"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 為什麼要閱讀文本文件？

閱讀文本文件對於任何一位有興趣學習編程的人來說都是一個必要的基礎技能。文本文件是儲存數據的常用格式，且在任何程式語言中都有其獨特的用途。透過學習如何讀取文本文件，你將能更好地處理和操作數據。

## 如何閱讀文本文件？

閱讀文本文件可以在Ruby中使用內建的File類別和gets方法。以下是一個簡單的程式碼範例：

```Ruby

file = File.open("text_file.txt", "r") # 開啟文本文件，read-only模式

file.each_line do |line| # 使用each_line方法遍歷文本文件的每一行
  puts line # 輸出每一行的內容
end

file.close # 關閉文件

```

當執行以上程式碼後，你將在終端機看到文本文件的每一行被依序輸出。你也可以使用其他方法來讀取和處理文本文件的內容，這些都是根據你的需要和想法來選擇的。

## 深入閱讀文本文件

除了使用內建的File類別和方法來讀取文本文件外，你也可以使用其他Ruby擴展庫來讀取不同格式的文本文件，例如CSV、JSON等。另外，你也可以了解如何處理文本文件中的特殊字符，以及如何使用正則表達式來處理文本內容。

## 請參考以下連結學習更多關於閱讀文本文件的知識：

- [Ruby文件操作指南](https://www.rubyguides.com/ruby-file/) 
- [Ruby的File類別文檔](https://ruby-doc.org/core-2.7.1/File.html) 
- [使用CSV庫處理文本文件](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html) 
- [深入了解正則表達式](https://ruby-doc.org/core-2.7.1/Regexp.html)

## 請看參考資料

- [Ruby文件操作指南](https://www.rubyguides.com/ruby-file/)
- [Ruby的File类别文档](https://ruby-doc.org/core-2.7.1/File.html)
- [使用CSV库处理文本文件](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [深入了解正则表达式](https://ruby-doc.org/core-2.7.1/Regexp.html)