---
title:    "Haskell: 두 날짜 비교하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜
특정 날짜의 비교를 진행하는 이유는 다양한 판단과 결정을 하기 위해서 입니다.

## 방법
비교하고자 하는 두 날짜를 입력하고, 그 결과를 반환하는 간단한 Haskell 함수를 작성하는 방법을 살펴보겠습니다.

```Haskell
compareDate :: Int -> Int -> IO ()
compareDate date1 date2 = do
    if date1 < date2 then
        putStrLn "Date 1 is before Date 2"
    else if date1 > date2 then
        putStrLn "Date 1 is after Date 2"
    else
        putStrLn "Date 1 and Date 2 are the same"
```

위 예제에서는 사용자로부터 두 개의 날짜를 입력받고, 입력된 날짜들을 비교하여 결과를 출력합니다. 우선 `compareDate` 함수를 정의한 뒤, `Int` 타입의 두 날짜를 매개변수로 받습니다. 함수 안에서는 입력받은 날짜에 대해 조건문을 사용하여 어떤 날짜가 다른 날짜보다 빠른지 늦은지, 또는 동일한지 판단합니다. 만약 첫 번째 날짜가 두 번째 날짜보다 빠르다면 "Date 1 is before Date 2"를 출력하고, 반대라면 "Date 1 is after Date 2"를 출력합니다. 두 날짜가 같다면 "Date 1 and Date 2 are the same"를 출력합니다.

요약하자면, 날짜를 비교하려면 두 날짜를 입력받고, 조건문을 사용하여 비교해야 합니다. 이를 위해 `compareDate` 함수를 작성하고, 입력받은 날짜에 따라 적절한 결과를 출력하도록 합니다.

## 딥 다이브
날짜를 비교하는 방법은 크게 두 가지로 나눌 수 있습니다. 첫 번째는 `compare` 함수를 사용하는 방법이고, 두 번째는 각 날짜를 따로 분리하여 비교하는 방법입니다.

`compare` 함수의 경우, `date1 < date2`와 같이 두 날짜를 비교하여 `LT`, `EQ`, `GT` 중 하나를 반환합니다. 이를 통해 비교한 날짜가 어떤 조건을 만족하는지 확인할 수 있습니다. 반면 날짜를 분리하여 비교하는 방법은 `year`, `month`, `day` 등의 요소를 각각 비교하는 것을 말합니다. 이 경우 `if`문을 사용하거나 `compare` 함수를 사용하여 여러 요소를 한 번에 비교할 수 있습니다.

날짜를 비교하는 방법은 단순하지만, 디테일하게 살펴보면 다양한 방법이 있으므로 적절한 방법을 선택하여 사용하면 됩니다.

## 참고
- [Hackage: Data.Time.Calendar](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)
- [Hoogle: compare](https://hoogle.haskell.org/?hoogle=compare)
- [Learn You a Haskell: Types and Typeclasses](http://learnyouahaskell.com/types-and-typeclasses)