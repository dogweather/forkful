---
title:                "Viết các bài kiểm tra"
aliases:
- /vi/clojure/writing-tests/
date:                  2024-01-28T22:13:18.016567-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/clojure/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
