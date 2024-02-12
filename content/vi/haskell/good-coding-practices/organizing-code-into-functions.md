---
title:                "Sắp xếp mã thành các hàm"
aliases: - /vi/haskell/organizing-code-into-functions.md
date:                  2024-01-28T22:03:39.487993-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tổ chức mã thành các hàm trong Haskell có nghĩa là bạn sẽ phân chia mã của mình thành các khối được đặt tên, có thể tái sử dụng. Tại sao lại như vậy? Điều này giữ cho mã của bạn không bị lặp lại (Don't Repeat Yourself - DRY), khiến nó dễ đọc và dễ gỡ lỗi hơn.

## Làm thế nào:
Dưới đây là cách bạn có thể viết và sử dụng các hàm trong Haskell:

```Haskell
-- Định nghĩa một hàm đơn giản để cộng hai số
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Sử dụng hàm
main = print (addNumbers 3 5)
```

Kết quả:
```
8
```

Bạn cũng có thể tạo các hàm bậc cao:

```Haskell
-- Lấy một hàm và áp dụng nó hai lần cho một thứ gì đó
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Sử dụng applyTwice với một hàm vô danh
main = print (applyTwice (*2) 5)
```

Kết quả:
```
20
```

## Sâu hơn
Haskell, một ngôn ngữ hoàn toàn hàm, coi hàm như những công dân hạng nhất. Về mặt lịch sử, điều này có nguồn gốc từ tính toán lambda, một khuôn khổ cơ bản trong lập trình. Không giống như các ngôn ngữ tường minh, nơi mà hàm là một chuỗi các chỉ thị, trong Haskell, hàm là những biểu thức mô tả mối quan hệ giữa dữ liệu.

Có những phương án khác để viết hàm tái sử dụng. Hãy cân nhắc sử dụng các lớp kiểu cho tính đa hình hoặc tận dụng các mô-đun để nhóm các hàm liên quan. Đánh giá trễ của Haskell cũng ảnh hưởng đến việc thực hiện hàm - các hàm sẽ không được đánh giá cho đến khi kết quả của chúng được cần, có thể ảnh hưởng đến xem xét về hiệu năng.

## Xem thêm
- Tài liệu chính thức của Haskell: https://www.haskell.org/documentation/
- "Learn You a Haskell for Great Good!" bởi Miran Lipovača, một quyển sách thân thiện với người mới bắt đầu: http://learnyouahaskell.com/
- "Real World Haskell" bởi Bryan O'Sullivan, Don Stewart, và John Goerzen: http://book.realworldhaskell.org/
