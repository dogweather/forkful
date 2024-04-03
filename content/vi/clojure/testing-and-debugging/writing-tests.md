---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:18.016567-07:00
description: "C\xE1ch th\u1EE9c: Clojure s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7\
  n g\u1ECDi l\xE0 `clojure.test` \u0111\u1EC3 vi\u1EBFt v\xE0 ch\u1EA1y th\u1EED\
  \ nghi\u1EC7m. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch s\u1EED d\u1EE5ng n\xF3."
lastmod: '2024-03-13T22:44:36.157981-06:00'
model: gpt-4-0125-preview
summary: "Clojure s\u1EED d\u1EE5ng m\u1ED9t th\u01B0 vi\u1EC7n g\u1ECDi l\xE0 `clojure.test`\
  \ \u0111\u1EC3 vi\u1EBFt v\xE0 ch\u1EA1y th\u1EED nghi\u1EC7m."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
