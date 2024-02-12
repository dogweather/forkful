---
title:                "Làm tròn số"
aliases:
- /vi/clojure/rounding-numbers.md
date:                  2024-01-28T22:06:59.096391-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm tròn số"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?
Làm tròn số là việc điều chỉnh một số để gần với số nguyên hơn, hoặc đến một độ chính xác thập phân nhất định. Chúng ta làm tròn số để đơn giản hóa chúng cho việc đọc của con người, giảm tải tính toán, hoặc đáp ứng các yêu cầu số lượng cụ thể.

## Làm thế nào:
Trong Clojure, chúng ta chủ yếu sử dụng `Math/round`, `Math/floor`, và `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Để làm tròn đến các vị trí thập phân cụ thể, chúng ta nhân, làm tròn và chia:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Sâu hơn
Trước khi có những ngôn ngữ lập trình tinh vi, việc làm tròn là một quy trình thủ công, hãy nghĩ về bàn tính hoặc giấy. Trong lập trình, nó rất quan trọng đối với việc biểu diễn số do giới hạn độ chính xác của số dấu phẩy động.

Các phương án thay thế để làm tròn bao gồm sử dụng lớp `BigDecimal` để kiểm soát độ chính xác hoặc thư viện như `clojure.math.numeric-tower` cho các chức năng toán học nâng cao. `Math/round` của Clojure dựa vào các hàm `Math.round`, `Math/floor`, và `Math/ceil` của Java, có nghĩa là nó thừa hưởng các vấn đề về số dấu phẩy động và số double.

Về việc triển khai, khi làm tròn trong Clojure, hãy nhớ rằng nó tự động sử dụng độ chính xác double khi xử lý với số thập phân. Hãy cẩn thận với lỗi làm tròn!

## Xem thêm
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Hiểu về Độ Chính Xác Dấu Phẩy Động: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
