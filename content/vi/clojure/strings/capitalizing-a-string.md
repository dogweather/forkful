---
title:                "Viết hoa một chuỗi"
aliases:
- /vi/clojure/capitalizing-a-string/
date:                  2024-01-28T21:55:47.634090-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
