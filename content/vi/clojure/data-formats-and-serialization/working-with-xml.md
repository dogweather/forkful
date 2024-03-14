---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:27.423702-07:00
description: "XML l\xE0 m\u1ED9t ng\xF4n ng\u1EEF \u0111\xE1nh d\u1EA5u \u0111\u01B0\
  \u1EE3c thi\u1EBFt k\u1EBF \u0111\u1EC3 m\xE3 h\xF3a t\xE0i li\u1EC7u m\u1ED9t c\xE1\
  ch c\xF3 th\u1EC3 \u0111\u1ECDc \u0111\u01B0\u1EE3c b\u1EDFi c\u1EA3 con ng\u01B0\
  \u1EDDi v\xE0 m\xE1y m\xF3c. N\xF3 \u0111\xF3ng m\u1ED9t vai tr\xF2 ch\xEDnh trong\
  \ c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.184950-06:00'
model: gpt-4-0125-preview
summary: "XML l\xE0 m\u1ED9t ng\xF4n ng\u1EEF \u0111\xE1nh d\u1EA5u \u0111\u01B0\u1EE3\
  c thi\u1EBFt k\u1EBF \u0111\u1EC3 m\xE3 h\xF3a t\xE0i li\u1EC7u m\u1ED9t c\xE1ch\
  \ c\xF3 th\u1EC3 \u0111\u1ECDc \u0111\u01B0\u1EE3c b\u1EDFi c\u1EA3 con ng\u01B0\
  \u1EDDi v\xE0 m\xE1y m\xF3c. N\xF3 \u0111\xF3ng m\u1ED9t vai tr\xF2 ch\xEDnh trong\
  \ c\xE1c\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
---

{{< edit_this_page >}}

## Làm gì & Tại sao?
XML là một ngôn ngữ đánh dấu được thiết kế để mã hóa tài liệu một cách có thể đọc được bởi cả con người và máy móc. Nó đóng một vai trò chính trong các dịch vụ web, tệp cấu hình, và trao đổi dữ liệu vì nó chuyên chở dữ liệu theo một định dạng có cấu trúc, phân cấp.

## Cách thực hiện:
Clojure cung cấp thư viện `clojure.data.xml` cho việc phân tích cú pháp và phát sinh XML. Đầu tiên, hãy phân tích cú pháp một số XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Phân tích cú pháp chuỗi XML
  (println parsed))
```
Đầu ra:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Để phát sinh XML từ các cấu trúc Clojure:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Đầu ra:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Sâu hơn
XML đã xuất hiện từ cuối những năm 90, là một tập con đơn giản của SGML, dành cho dữ liệu web. Nó đã phát triển mạnh mẽ với các công nghệ như SOAP và XHTML nhưng cũng gặp phải sự cạnh tranh từ JSON, được ưa chuộng vì tính đơn giản và nhẹ nhàng.

Cách tiếp cận XML của Clojure giữ cho nó mang tính chức năng và tập trung vào dữ liệu, trung thành với tinh thần của ngôn ngữ. `clojure.data.xml` chỉ là một trong những lựa chọn; bạn cũng có `clojure.xml` cho nhu cầu cơ bản, và đối với tương tác Java, bạn có thể sử dụng những lựa chọn mạnh mẽ như JAXB hoặc DOM4J.

Hãy nhớ, hiệu suất và overhead bộ nhớ khi xử lý với các tài liệu XML rất lớn có thể nặng nề. Các trình phân tích cú pháp dạng luồng như StAX có thể giúp ích, nhưng bạn sẽ cần chuyển sang Java-land cho chúng.

## Xem thêm
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
