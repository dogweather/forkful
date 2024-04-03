---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:16.764422-07:00
description: "L\xE0m th\u1EBF n\xE0o: Haskell x\u1EED l\xFD l\u1ED7i m\u1ED9t c\xE1\
  ch m\u1EA1nh m\u1EBD th\xF4ng qua c\xE1c ki\u1EC3u nh\u01B0 `Maybe` v\xE0 `Either`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1i nh\xECn nhanh."
lastmod: '2024-03-13T22:44:36.721834-06:00'
model: gpt-4-0125-preview
summary: "Haskell x\u1EED l\xFD l\u1ED7i m\u1ED9t c\xE1ch m\u1EA1nh m\u1EBD th\xF4\
  ng qua c\xE1c ki\u1EC3u nh\u01B0 `Maybe` v\xE0 `Either`."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

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
