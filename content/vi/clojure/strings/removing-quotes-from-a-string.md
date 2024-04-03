---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:05.653539-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Clojure, chu\u1ED7i l\xE0 b\u1EA5\
  t bi\u1EBFn, v\xEC v\u1EADy khi ch\xFAng ta n\xF3i v\u1EC1 \"lo\u1EA1i b\u1ECF d\u1EA5\
  u ngo\u1EB7c\", ch\xFAng ta th\u1EF1c s\u1EF1 \u0111ang n\xF3i v\u1EC1 vi\u1EC7\
  c t\u1EA1o ra m\u1ED9t chu\u1ED7i m\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.137017-06:00'
model: gpt-4-0125-preview
summary: "Trong Clojure, chu\u1ED7i l\xE0 b\u1EA5t bi\u1EBFn, v\xEC v\u1EADy khi ch\xFA\
  ng ta n\xF3i v\u1EC1 \"lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c\", ch\xFAng ta th\u1EF1\
  c s\u1EF1 \u0111ang n\xF3i v\u1EC1 vi\u1EC7c t\u1EA1o ra m\u1ED9t chu\u1ED7i m\u1EDB\
  i kh\xF4ng c\xF3 d\u1EA5u ngo\u1EB7c."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Cách thực hiện:
Trong Clojure, chuỗi là bất biến, vì vậy khi chúng ta nói về "loại bỏ dấu ngoặc", chúng ta thực sự đang nói về việc tạo ra một chuỗi mới không có dấu ngoặc. Dưới đây là cách thực hiện sử dụng `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Hãy loại bỏ những dấu ngoặc kép
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; Và loại bỏ dấu ngoặc đơn
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Ví dụ sử dụng:
(remove-double-quotes "\"Xin chào, Thế giới!\"") ; => "Xin chào, Thế giới!"
(remove-single-quotes "'Xin chào, Thế giới!'")   ; => "Xin chào, Thế giới!"
```
Muốn xử lý cả dấu ngoặc đơn và dấu ngoặc kép trong một lúc? Hãy xem cái này:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Ví dụ sử dụng:
(remove-quotes "\"Xin chào, 'Clojure' Thế giới!\"") ; => "Xin chào, Clojure Thế giới!"
```

## Sâu hơn nữa
Ngày xưa, khi dữ liệu còn rối bời như phòng của một đứa trẻ, dấu ngoặc trong chuỗi là điều bình thường để biểu thị văn bản. Nhưng khi khoa học máy tính phát triển, dấu ngoặc trở thành nhiều hơn chỉ là dấu giới hạn văn bản - chúng đóng vai trò cú pháp trong các ngôn ngữ lập trình.

Clojure, với di sản Lisp của mình, không sử dụng dấu ngoặc giống như một số ngôn ngữ khác có thể. Chúng chắc chắn được sử dụng để biểu thị chuỗi, nhưng chúng còn có một vai trò đặc biệt trong việc tạo ra các literal. Dù vậy, việc loại bỏ dấu ngoặc khỏi chuỗi vẫn là một nhiệm vụ không lỗi thời.

Tại sao không chỉ cắt bỏ hai đầu của chuỗi? Vâng, đó là giả sử rằng dấu ngoặc của bạn luôn ôm lấy đầu và cuối chuỗi của bạn như một cặp ông bà quá đỗi yêu thương. Dữ liệu thế giới thực thì rối bời hơn nhiều. Hãy nhắc đến regex (biểu thức chính quy), cho phép bạn nhắm vào những dấu ngoặc dù chúng nằm ở đâu.

Các phương án khác? Chắc chắn, bạn có thể trở nên tinh tế với `subs`, `trim`, `triml`, `trimr`, hoặc thậm chí transducers nếu bạn muốn khoe. Nhưng `replace` với regex giống như mang một thanh kiếm ánh sáng đến một trận chiến dao - nó cắt ngay vào vấn đề.

## Xem thêm
Nếu bộ não của bạn đang thèm khát thêm nhiều điều tốt lành về thao tác chuỗi Clojure, những manh mối này có thể giúp:

- ClojureDocs về `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Biểu thức chính quy trong Clojure: https://clojure.org/guides/learn/syntax#_regex
- Java interop cho việc xử lý chuỗi (dù sao Clojure cũng chạy trên JVM): https://clojure.org/reference/java_interop#_working_with_strings

Đừng chỉ dừng lại ở việc loại bỏ dấu ngoặc. Cả một thế giới phép thuật chuỗi đang chờ được khám phá ngoài kia trong vùng đất Clojure.
