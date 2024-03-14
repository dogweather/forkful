---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:49.513048-07:00
description: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i c\xF3 ngh\u0129\
  a l\xE0 b\u1EA1n \u0111\xE3 trang b\u1ECB cho m\xECnh m\u1ED9t chi\u1EBFc k\xED\
  nh l\xFAp \u0111\u1EC3 xem x\xE9t m\xE3 c\u1EE7a m\xECnh. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 lo\u1EA1i b\u1ECF l\u1ED7i,\u2026"
lastmod: '2024-03-13T22:44:36.159230-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng m\u1ED9t tr\xECnh g\u1EE1 l\u1ED7i c\xF3 ngh\u0129a l\xE0\
  \ b\u1EA1n \u0111\xE3 trang b\u1ECB cho m\xECnh m\u1ED9t chi\u1EBFc k\xEDnh l\xFA\
  p \u0111\u1EC3 xem x\xE9t m\xE3 c\u1EE7a m\xECnh. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 lo\u1EA1i b\u1ECF l\u1ED7i,\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Sử dụng một trình gỡ lỗi có nghĩa là bạn đã trang bị cho mình một chiếc kính lúp để xem xét mã của mình. Các lập trình viên làm điều này để loại bỏ lỗi, hiểu dòng chảy, và đảm bảo logic của họ diễn ra như mong đợi.

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
