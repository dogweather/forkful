---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:13.349402-07:00
description: "In th\xF4ng tin g\u1EE1 r\u1ED1i l\xE0 vi\u1EC7c phun ra d\u1EEF li\u1EC7\
  u t\u1EEB ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n \u0111\u1EC3 xem chuy\u1EC7\
  n g\xEC \u0111ang x\u1EA3y ra ph\xEDa sau. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo\u2026"
lastmod: '2024-03-13T22:44:36.715408-06:00'
model: gpt-4-0125-preview
summary: "In th\xF4ng tin g\u1EE1 r\u1ED1i l\xE0 vi\u1EC7c phun ra d\u1EEF li\u1EC7\
  u t\u1EEB ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n \u0111\u1EC3 xem chuy\u1EC7\
  n g\xEC \u0111ang x\u1EA3y ra ph\xEDa sau. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Gì và Tại Sao?

In thông tin gỡ rối là việc phun ra dữ liệu từ chương trình của bạn để xem chuyện gì đang xảy ra phía sau. Các lập trình viên thực hiện điều này để theo dõi các biến, hiểu luồng và loại bỏ những lỗi phiền phức.

## Làm thế nào:

Một cách dễ dàng để in thông tin gỡ rối trong Haskell là sử dụng hàm `print`, nó nhận một giá trị là thể hiện của typeclass `Show` và xuất nó ra console.

```Haskell
main :: IO ()
main = do
  let number = 42
  print number
  putStrLn "Gỡ rối thật là dễ dàng trong Haskell!"

-- Đầu ra:
-- 42
-- Gỡ rối thật là dễ dàng trong Haskell!
```

Đối với các cấu trúc dữ liệu phức tạp hơn, đảm bảo chúng được phát sinh từ `Show` để kích hoạt in đẹp:

```Haskell
data Cake = Chocolate | Vanilla deriving Show

debugFlavor :: Cake -> IO ()
debugFlavor flavor = print flavor

main :: IO ()
main = debugFlavor Chocolate

-- Đầu ra:
-- Chocolate
```

Đôi khi chúng ta muốn gỡ rối tạm thời mà dễ dàng loại bỏ sau này. Hãy nhập mô-đun `Debug.Trace`.

```Haskell
import Debug.Trace (trace)

main :: IO ()
main = putStrLn $ trace "Điều này sẽ được in trước" "Điều này sẽ được in sau"

-- Đầu ra:
-- Điều này sẽ được in trước
-- Điều này sẽ được in sau
```

Hàm `trace` in chuỗi khi giá trị được đánh giá, nhưng nó là một tác dụng phụ trong phần mã nguyên thủy. Nó rất hữu ích nhưng hãy sử dụng với sự cẩn thận!

## Sâu hơn

Trong những ngày xưa cũ, gỡ rối có thể là chiêu trò "in câu lệnh" cũ kỹ. Haskell cung cấp điều này với một chút xoay chuyển chức năng và công cụ cho các thực hành gỡ rối sạch hơn. Hãy gặp hàm `print` và mô-đun `Debug.Trace`, như đã khám phá trước đây.

Những lựa chọn thay thế cho `print` bao gồm `putStrLn` cho chuỗi và `putStr`, nếu bạn không thích dấu xuống dòng tự động đó. `Debug.Trace` cũng có các biến thể như `traceShow` làm việc trực tiếp với các thể hiện của `Show`, tiết kiệm cho bạn một cuộc gọi `show`.

Về chi tiết triển khai, `print` cơ bản là `putStrLn . show`. Nó in bất kỳ dữ liệu `Show`-able nào ra stdout. Các hàm `Debug.Trace`, ngược lại, dành cho việc sử dụng tạm thời trong quá trình phát triển. Chúng len lỏi vào mã nguyên thủy và vi phạm tính minh bạch tham chiếu, đó là điều không ổn trong dài hạn.

Đừng quên các thư viện ghi nhật ký cho các ứng dụng nghiêm túc, mang lại nhiều kiểm soát hơn và ít "gỡ rối bằng cách in" hơn.

## Xem Thêm

- Tài liệu `Debug.Trace`: [https://hackage.haskell.org/package/base/docs/Debug-Trace.html](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)
- Haskell Wiki về Gỡ rối: [https://wiki.haskell.org/Debugging](https://wiki.haskell.org/Debugging)
- Một cuộc thảo luận hay về việc tại sao không sử dụng `Debug.Trace` và nên làm gì thay thế: [https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice](https://stackoverflow.com/questions/7741400/why-is-using-debug-trace-considered-bad-practice)
