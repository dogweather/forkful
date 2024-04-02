---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:29.669205-07:00
description: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong l\u1EADp tr\xEC\
  nh l\xE0 v\u1EC1 vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB kh\xF4ng th\u1EC3\
  \ \u0111\u01B0\u1EE3c d\u1EF1 \u0111o\xE1n m\u1ED9t c\xE1ch logic tr\u01B0\u1EDB\
  c th\u1EDDi \u0111i\u1EC3m. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c\u2026"
lastmod: '2024-03-13T22:44:36.147478-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong l\u1EADp tr\xECnh l\xE0\
  \ v\u1EC1 vi\u1EC7c t\u1EA1o ra c\xE1c gi\xE1 tr\u1ECB kh\xF4ng th\u1EC3 \u0111\u01B0\
  \u1EE3c d\u1EF1 \u0111o\xE1n m\u1ED9t c\xE1ch logic tr\u01B0\u1EDBc th\u1EDDi \u0111\
  i\u1EC3m. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Là gì & Tại sao?

Việc tạo số ngẫu nhiên trong lập trình là về việc tạo ra các giá trị không thể được dự đoán một cách logic trước thời điểm. Lập trình viên thực hiện việc này với nhiều lý do khác nhau, bao gồm tạo ra các định danh duy nhất, mô phỏng các kịch bản trong phát triển trò chơi, hoặc chọn mẫu ngẫu nhiên từ dữ liệu để phân tích.

## Làm thế nào:

Trong Clojure, việc tạo số ngẫu nhiên khá đơn giản, và có một số hàm được tích hợp sẵn có thể được sử dụng ngay lập tức.

Để tạo một số thực ngẫu nhiên giữa 0 (bao gồm) và 1 (không bao gồm), bạn có thể sử dụng hàm `rand`:

```Clojure
(rand)
;; Ví dụ đầu ra: 0.7094245047062917
```

Nếu bạn cần một số nguyên trong một phạm vi cụ thể, sử dụng `rand-int`:

```Clojure
(rand-int 10)
;; Ví dụ đầu ra: 7
```

Điều này cho bạn một số nguyên ngẫu nhiên giữa 0 (bao gồm) và số bạn truyền làm đối số (không bao gồm).

Để tạo một số ngẫu nhiên trong một phạm vi cụ thể (không giới hạn ở số nguyên), bạn có thể kết hợp `rand` với phép toán:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Sử dụng
(rand-range 10 20)
;; Ví dụ đầu ra: 14.857457734992847
```

Hàm `rand-range` này sẽ trả về một số thực ngẫu nhiên giữa các giá trị `min` và `max` bạn chỉ định.

Đối với các tình huống cần các phân phối phức tạp hơn hoặc các chuỗi số ngẫu nhiên mà khả năng lặp lại là cần thiết (sử dụng hạt giống), bạn có thể cần tìm hiểu thêm về các thư viện bổ sung nằm ngoài những gì được tích hợp sẵn.

## Sâu hơn

Cơ chế cơ bản để tạo số ngẫu nhiên trong hầu hết các ngôn ngữ lập trình, bao gồm Clojure, thường dựa vào bộ sinh số giả ngẫu nhiên (PRNG). Một PRNG sử dụng một thuật toán để tạo ra một chuỗi số mô phỏng các tính chất của số ngẫu nhiên. Đáng chú ý là do các số này được tạo ra bằng thuật toán, chúng không thực sự ngẫu nhiên nhưng có thể đủ cho hầu hết các mục đích thực tế.

Trong những ngày đầu của máy tính, việc tạo ra số ngẫu nhiên chất lượng cao là một thách thức lớn, dẫn đến việc phát triển các thuật toán khác nhau để cải thiện độ ngẫu nhiên và phân phối. Đối với Clojure, các hàm được tích hợp sẵn, như `rand` và `rand-int`, tiện lợi cho việc sử dụng hàng ngày và đáp ứng một phạm vi rộng lớn của các trường hợp sử dụng thường gặp.

Tuy nhiên, đối với các ứng dụng yêu cầu bảo mật mật mã hoặc các phương pháp lấy mẫu thống kê phức tạp hơn, các nhà phát triển Clojure thường chuyển sang các thư viện bên ngoài cung cấp các PRNG mạnh mẽ và chuyên biệt hơn. Các thư viện như `clj-random` cung cấp quyền truy cập vào nhiều thuật toán hơn và kiểm soát lớn hơn đối với việc seeding, điều này có thể rất quan trọng cho các mô phỏng, ứng dụng mật mã, hoặc bất kỳ lĩnh vực nào mà chất lượng và khả năng dự đoán của chuỗi số ngẫu nhiên có thể có ý nghĩa quan trọng.

Mặc dù các khả năng tích hợp sẵn của Clojure trong việc tạo số ngẫu nhiên là đủ cho nhiều nhiệm vụ, nhưng việc khám phá các thư viện bên ngoài có thể cung cấp cái nhìn sâu rộng hơn và các tùy chọn cho các ứng dụng được cá nhân hóa hoặc quan trọng hơn.
