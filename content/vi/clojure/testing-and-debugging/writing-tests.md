---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:18.016567-07:00
description: "Vi\u1EBFt test c\xF3 ngh\u0129a l\xE0 t\u1EA1o ra m\xE3 l\u1EC7nh ki\u1EC3\
  m tra xem m\xE3 l\u1EC7nh kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 mong \u0111\
  \u1EE3i hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn l\xE0m v\u1EADy \u0111\u1EC3 b\u1EAF\
  t l\u1ED7i, \u0111\u1EA3m b\u1EA3o \u0111\u1ED9 tin c\u1EADy\u2026"
lastmod: '2024-03-13T22:44:36.157981-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt test c\xF3 ngh\u0129a l\xE0 t\u1EA1o ra m\xE3 l\u1EC7nh ki\u1EC3\
  m tra xem m\xE3 l\u1EC7nh kh\xE1c c\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 mong \u0111\
  \u1EE3i hay kh\xF4ng. L\u1EADp tr\xECnh vi\xEAn l\xE0m v\u1EADy \u0111\u1EC3 b\u1EAF\
  t l\u1ED7i, \u0111\u1EA3m b\u1EA3o \u0111\u1ED9 tin c\u1EADy\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Cái gì & Tại sao?
Viết test có nghĩa là tạo ra mã lệnh kiểm tra xem mã lệnh khác có hoạt động như mong đợi hay không. Lập trình viên làm vậy để bắt lỗi, đảm bảo độ tin cậy và tránh nhức đầu sau này.

## Cách thức:
Clojure sử dụng một thư viện gọi là `clojure.test` để viết và chạy thử nghiệm. Dưới đây là cách sử dụng nó:

```Clojure
(require '[clojure.test :refer :all])

(deftest addition-test
  (testing "Phép cộng cơ bản"
    (is (= 4 (+ 2 2)))))
    
(run-tests)
```

Kết quả mẫu sau khi chạy thử nghiệm:

```
lein test user
Testing user

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Sâu hơn
Phương pháp kiểm thử của Clojure bắt nguồn từ môi trường phát triển dựa vào REPL. Kiểm thử sinh sản với `test.check` và kiểm thử dựa trên đặc tính là các chiến lược thay thế. Chúng tự động tạo ra các trường hợp kiểm thử thay vì phải viết tất cả bằng tay. Việc triển khai phụ thuộc nhiều vào các macro, cung cấp một môi trường kiểm thử động.

## Xem thêm
- [Kiểm thử Clojure](https://clojure.org/guides/deps_and_cli#_testing)
- [Tài liệu clojure.test trên GitHub](https://github.com/clojure/clojure/blob/master/src/clj/clojure/test.clj)
- [Giới thiệu về kiểm thử dựa trên đặc tính với `test.check`](https://github.com/clojure/test.check)
