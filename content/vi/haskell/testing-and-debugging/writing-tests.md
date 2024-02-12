---
title:                "Viết các bài kiểm tra"
aliases: - /vi/haskell/writing-tests.md
date:                  2024-01-28T22:13:26.876517-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/haskell/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
