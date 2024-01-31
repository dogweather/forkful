---
title:                "Làm việc với XML"
date:                  2024-01-28T22:11:27.423702-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
