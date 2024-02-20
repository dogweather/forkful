---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:26.876517-07:00
description: "Vi\u1EBFt test ki\u1EC3m tra xem code c\xF3 th\u1EF1c hi\u1EC7n \u0111\
  \xFAng c\xF4ng vi\u1EC7c m\xE0 n\xF3 c\u1EA7n l\xE0m hay kh\xF4ng. Testing gi\xFA\
  p b\u1EAFt l\u1ED7i, h\u1ED7 tr\u1EE3 b\u1EA3o tr\xEC code, v\xE0 \u0111\u1EA3m\
  \ b\u1EA3o c\xE1c thay \u0111\u1ED5i kh\xF4ng\u2026"
lastmod: 2024-02-19 22:04:55.895947
model: gpt-4-0125-preview
summary: "Vi\u1EBFt test ki\u1EC3m tra xem code c\xF3 th\u1EF1c hi\u1EC7n \u0111\xFA\
  ng c\xF4ng vi\u1EC7c m\xE0 n\xF3 c\u1EA7n l\xE0m hay kh\xF4ng. Testing gi\xFAp b\u1EAF\
  t l\u1ED7i, h\u1ED7 tr\u1EE3 b\u1EA3o tr\xEC code, v\xE0 \u0111\u1EA3m b\u1EA3o\
  \ c\xE1c thay \u0111\u1ED5i kh\xF4ng\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
---

{{< edit_this_page >}}

## Gì & Tại sao?

Viết test kiểm tra xem code có thực hiện đúng công việc mà nó cần làm hay không. Testing giúp bắt lỗi, hỗ trợ bảo trì code, và đảm bảo các thay đổi không làm hỏng mọi thứ.

## Cách thực hiện:

Haskell sử dụng HUnit cho các unit test cơ bản, và QuickCheck cho các property-based test. Dưới đây là một ví dụ nhanh về HUnit:

```haskell
import Test.HUnit

testList :: Test
testList = TestList [TestCase (assertEqual "Should add numbers" 4 (2 + 2)),
                     TestCase (assertEqual "Should subtract numbers" 0 (2 - 2))]

main :: IO Counts
main = runTestTT testList
```

Chạy nó, và nó hiển thị:

```
Cases: 2  Tried: 2  Errors: 0  Failures: 0
```

Ví dụ về QuickCheck:

```haskell
import Test.QuickCheck

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

main :: IO ()
main = quickCheck prop_RevRev
```

Kết quả mẫu có thể đọc là:

```
+++ OK, đã vượt qua 100 bài test.
```

## Tìm hiểu sâu hơn

Testing bắt đầu từ sớm khi lập trình ra đời, nhưng trở nên nghiêm túc với sự phát triển của TDD vào những năm 2000. Các hàm thuần túy của Haskell làm cho nó trở nên tuyệt vời cho việc testing. Các lựa chọn thay thế cho HUnit/QuickCheck bao gồm doctest và Hedgehog. HUnit giống như JUnit trong Java. QuickCheck tự động tạo các trường hợp test, kiểm tra các tính chất mà bạn định nghĩa.

## Xem thêm

- Tài liệu HUnit: [http://hackage.haskell.org/package/HUnit](http://hackage.haskell.org/package/HUnit)
- QuickCheck trên Hackage: [http://hackage.haskell.org/package/QuickCheck](http://hackage.haskell.org/package/QuickCheck)
- Giới thiệu về Testing trong Haskell: [https://hspec.github.io/](https://hspec.github.io/)
- "Real World Haskell" bởi Bryan O'Sullivan, Don Stewart, và John Goerzen: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
