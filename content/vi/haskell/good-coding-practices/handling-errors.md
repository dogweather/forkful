---
aliases:
- /vi/haskell/handling-errors/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:16.764422-07:00
description: "X\u1EED l\xFD l\u1ED7i trong l\u1EADp tr\xECnh l\xE0 v\u1EC1 vi\u1EC7\
  c qu\u1EA3n l\xFD nh\u1EEFng \u0111i\u1EC1u kh\xF4ng mong \u0111\u1EE3i\u2014nh\u1EEF\
  ng \u0111i\u1EC1u c\xF3 th\u1EC3 \u0111i sai. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o r\u1EB1ng ch\u01B0\u01A1ng tr\xEC\
  nh\u2026"
lastmod: 2024-02-18 23:08:50.753127
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i trong l\u1EADp tr\xECnh l\xE0 v\u1EC1 vi\u1EC7c qu\u1EA3\
  n l\xFD nh\u1EEFng \u0111i\u1EC1u kh\xF4ng mong \u0111\u1EE3i\u2014nh\u1EEFng \u0111\
  i\u1EC1u c\xF3 th\u1EC3 \u0111i sai. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 \u0111\u1EA3m b\u1EA3o r\u1EB1ng ch\u01B0\u01A1ng tr\xECnh\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Xử lý lỗi trong lập trình là về việc quản lý những điều không mong đợi—những điều có thể đi sai. Lập trình viên làm điều này để đảm bảo rằng chương trình của họ có thể đối phó với những tình huống này một cách duyên dáng, không bị sụp đổ hoặc sản xuất kết quả sai.

## Làm thế nào:
Haskell xử lý lỗi một cách mạnh mẽ thông qua các kiểu như `Maybe` và `Either`. Dưới đây là một cái nhìn nhanh:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Chia cho không là không được, vì thế chúng ta trả về Nothing.
safeDivide x y = Just (x `div` y)  -- Nếu không, mọi thứ đều ổn, trả về kết quả trong Just.

-- Hãy xem nó hoạt động như thế nào:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Đối với việc xử lý lỗi phức tạp hơn, `Either` được sử dụng:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Lỗi chia cho không."  -- Lần này, lỗi mang một thông điệp.
safeDivideEither x y = Right (x `div` y)

-- Và khi sử dụng:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Lỗi chia cho không."
```

## Sâu hơn
Trong thế giới Haskell, xử lý lỗi có một lịch sử đáng kể. Ngày xưa, lỗi có thể làm sập toàn bộ chương trình của bạn—không vui vẻ gì cả. Hệ thống kiểu của Haskell cung cấp các cách làm cho điều này ít xảy ra hơn. Chúng ta có `Maybe` và `Either`, nhưng còn có những loại khác như `Exceptions` và `IO` cho các tình huống khác nhau.

`Maybe` là đơn giản: bạn nhận được `Just` điều gì đó nếu mọi thứ ổn, hoặc `Nothing` nếu không. `Either` nâng cao hơn, cho phép bạn trả về một thông điệp lỗi (`Left`) hoặc một kết quả thành công (`Right`).

Cả hai đều là pure, có nghĩa là chúng không can thiệp vào thế giới bên ngoài - một điều lớn lao trong Haskell. Chúng tôi tránh được những cạm bẫy của các ngoại lệ không được kiểm tra mà làm phiền một số ngôn ngữ khác.

Đối với những người không hài lòng với `Maybe` và `Either`, các thư viện như `Control.Exception` cung cấp xử lý lỗi theo phong cách lập trình thủ tục truyền thống qua các ngoại lệ. Nhưng sử dụng chúng một cách tự do quá mức có thể làm phức tạp mọi chuyện, vì vậy cộng đồng thường gắn bó với các kiểu.

## Xem thêm
Đào sâu hơn với:

- Tài liệu của chính Haskell: [Haskell](https://haskell.org/documentation)
- Tốt cho người mới bắt đầu: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
