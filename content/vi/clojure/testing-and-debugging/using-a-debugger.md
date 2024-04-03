---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:49.513048-07:00
description: "L\xE0m th\u1EBF n\xE0o: Clojure d\u1EF1a v\xE0o M\xE1y \u1EA3o Java\
  \ (JVM), v\xEC v\u1EADy nhi\u1EC1u vi\u1EC7c g\u1EE1 l\u1ED7i \u0111\u01B0\u1EE3\
  c th\u1EF1c hi\u1EC7n v\u1EDBi c\xE1c c\xF4ng c\u1EE5 Java. M\u1ED9t c\xF4ng c\u1EE5\
  \ nh\u01B0 th\u1EBF l\xE0 `CIDER`, m\u1ED9t g\xF3i m\u1EA1nh\u2026"
lastmod: '2024-03-13T22:44:36.159230-06:00'
model: gpt-4-0125-preview
summary: "Clojure d\u1EF1a v\xE0o M\xE1y \u1EA3o Java (JVM), v\xEC v\u1EADy nhi\u1EC1\
  u vi\u1EC7c g\u1EE1 l\u1ED7i \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n v\u1EDBi c\xE1\
  c c\xF4ng c\u1EE5 Java."
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

## Làm thế nào:
Clojure dựa vào Máy ảo Java (JVM), vì vậy nhiều việc gỡ lỗi được thực hiện với các công cụ Java. Một công cụ như thế là `CIDER`, một gói mạnh mẽ cho phát triển Clojure trong Emacs, có khả năng gỡ lỗi vững chắc. Cùng khám phá:

```clojure
;; Đầu tiên, kết nối vào một dự án Clojure trong Emacs sử dụng CIDER
M-x cider-jack-in

;; Đặt một điểm dừng
;; Di chuyển đến dòng trong mã Clojure bạn muốn kiểm tra và
;; nhấn "C-c M-b" hoặc thực thi:
M-x cider-debug-defun-at-point

;; Khi mã chạy, bạn sẽ chạm vào điểm dừng. CIDER sẽ yêu cầu bạn với:
;; 1. n để đi đến bước lógic tiếp theo trong việc thực thi,
;; 2. c để tiếp tục thực thi cho đến điểm dừng tiếp theo,
;; 3. q để thoát khỏi việc gỡ lỗi.

;; Kiểm tra biến cục bộ tại điểm dừng
;; Khi đang ở một điểm dừng, gõ:
locals

;; Bạn sẽ thấy một danh sách các biến cục bộ và giá trị của chúng được in trong minibuffer.
```
Kết quả mẫu có thể trông như thế này:
```clojure
{:x 10, :y 20, :result 200}
```

## Sâu hơn
Trình gỡ lỗi là một công cụ cũ như các ngọn đồi trong thuật ngữ máy tính. Thuật ngữ "bug" được đặt ra từ những ngày đầu của máy tính khi một con côn trùng thực sự gây ra lỗi bằng cách gây chập mạch trong một máy.

Trong khi `CIDER` tốt cho những người hâm mộ Emacs, có những lựa chọn khác cho việc gỡ lỗi Clojure. Ví dụ, sử dụng IntelliJ với plugin Cursive có thể cung cấp một trải nghiệm gỡ lỗi dựa trên GUI hơn. Ngoài ra, bạn có thể sử dụng Leiningen hoặc tools.deps được tích hợp sẵn để điều khiển quá trình dòng chảy khi gỡ lỗi.

Bên dưới cơ chế, các trình gỡ lỗi thường thao tác bytecodes, thực hiện đánh giá trong các phiên nREPL riêng biệt, và cung cấp kiểm tra ngăn xếp lỗi. Chúng đang tận dụng khả năng của JVM bên dưới, tìm cách vào kho lớn của các khung làm việc gỡ lỗi của Java.

## Xem thêm
- [Tài liệu Trình gỡ lỗi CIDER](https://docs.cider.mx/cider/debugging/debugger.html)
- [Trình gỡ lỗi Cursive](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen cho Tự động hóa và Gỡ lỗi](https://leiningen.org/)
- [tools.deps.alpha cho nhiều quyền kiểm soát hơn](https://github.com/clojure/tools.deps.alpha)
