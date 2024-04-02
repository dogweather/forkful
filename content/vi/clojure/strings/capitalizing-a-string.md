---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:47.634090-07:00
description: "Chuy\u1EC3n k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn c\u1EE7a chu\u1ED7i\
  \ th\xE0nh ch\u1EEF hoa v\xE0 ph\u1EA7n c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\
  \u1EDDng \u0111\u01B0\u1EE3c g\u1ECDi l\xE0 vi\u1EC7c vi\u1EBFt hoa chu\u1ED7i.\
  \ Ch\xFAng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 chu\u1EA9n\
  \ h\xF3a\u2026"
lastmod: '2024-03-13T22:44:36.130401-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n k\xFD t\u1EF1 \u0111\u1EA7u ti\xEAn c\u1EE7a chu\u1ED7i th\xE0\
  nh ch\u1EEF hoa v\xE0 ph\u1EA7n c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\u1EDD\
  ng \u0111\u01B0\u1EE3c g\u1ECDi l\xE0 vi\u1EC7c vi\u1EBFt hoa chu\u1ED7i. Ch\xFA\
  ng ta th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 chu\u1EA9n h\xF3a\u2026"
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Lý do & Tại sao?
Chuyển ký tự đầu tiên của chuỗi thành chữ hoa và phần còn lại thành chữ thường được gọi là việc viết hoa chuỗi. Chúng ta thực hiện điều này để chuẩn hóa dữ liệu và cải thiện khả năng đọc, như chuyển 'alice' thành 'Alice' cho tên.

## Cách thực hiện:
Trong Clojure, không có hàm được xây dựng sẵn để trực tiếp viết hoa chuỗi. Bạn tự tạo với thư viện `clojure.string`. Dưới đây là cách nhanh:

```clojure
(require '[clojure.string :as str])

(defn capitalize [s]
  (when s
    (str/capitalize s)))

(capitalize "hello world") ; => "Hello world"
```

Ví dụ đầu ra cho hàm `capitalize`:

```clojure
(capitalize "clojure") ; => "Clojure"
(capitalize "123clojure") ; => "123clojure"
(capitalize "") ; => nil
(capitalize nil) ; => nil
```

## Kỹ thuật chuyên sâu
Thư viện tiêu chuẩn của Clojure, `clojure.string`, ưu tiên sự đơn giản. Vì thế, không có hàm `capitalize` sẵn có như bạn có thể tìm thấy trong các ngôn ngữ khác. Về mặt lịch sử, Clojure dựa vào các phương thức String của Java, chúng cung cấp các thao tác cơ bản nhưng không có `capitalize`.

Thiếu sót này buộc bạn phải viết giải pháp của riêng mình, như ở trên, hoặc sử dụng các thư viện bên ngoài. Cũng có hàm `capitalize` từ `clojure.contrib.string`, thư viện contrib riêng biệt trước khi bị lỗi thời và một phần được hợp nhất với clojure.string trong các phiên bản sau.

Sự đơn giản của hàm `str/capitalize` có nghĩa là nó chỉ quan tâm đến ký tự đầu tiên. Đối với việc viết hoa tinh vi hơn, như việc viết hoa mỗi từ hay xử lý các ký tự quốc tế, bạn phải viết giải pháp tùy chỉnh hoặc sử dụng thư viện Java.

Dưới đây là một hàm tùy chỉnh khác xử lý chuỗi với nhiều từ:

```clojure
(defn title-case [s]
  (->> s
       (str/split #"\s+")
       (map str/capitalize)
       (str/join " ")))

(title-case "the lord of the rings") ; => "The Lord Of The Rings"
```

Lại một lần nữa, quốc tế hóa (i18n) không được bàn tới ở đây; việc xử lý Unicode một cách chính xác là một chủ đề hoàn toàn khác, thường yêu cầu các thư viện chuyên biệt.

## Xem thêm
- Clojure Strings API: https://clojure.github.io/clojure/clojure.string-api.html
- Tài liệu Java String: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Thư viện Clojure Contrib (đã lưu trữ): https://github.com/clojure/clojure-contrib
- Mã nguồn `clojure.string`: https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj
