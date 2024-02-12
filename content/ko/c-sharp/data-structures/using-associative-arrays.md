---
title:                "연관 배열 사용하기"
aliases:
- /ko/c-sharp/using-associative-arrays.md
date:                  2024-01-30T19:10:25.192856-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

C#에서의 연관 배열 또는 딕셔너리는 키와 값의 쌍을 저장하고 관리하게 해줍니다. 고유 식별자를 기반으로 값을 빠르게 가져와야 할 때 사용하며, 복잡한 애플리케이션에서의 데이터 관리를 손쉽게 해줍니다.

## 어떻게 사용하는가:

C#에서는 `Dictionary<TKey, TValue>` 클래스를 사용하여 연관 배열을 작업합니다. 시작하기에 앞서 간단한 예제를 보여드리겠습니다:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // 딕셔너리 생성
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // 키-값 쌍 추가
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // 키를 사용하여 값에 접근
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // 값 업데이트
        fruitBasket["Apples"] = 7;
        Console.WriteLine("업데이트된 Apples: " + fruitBasket["Apples"]);
        
        // 키-값 쌍 제거
        fruitBasket.Remove("Oranges");

        // 딕셔너리를 반복 처리
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
샘플 출력:
```
Apples: 5
업데이트된 Apples: 7
Apples: 7
```

이 예제는 딕셔너리의 생성, 추가, 접근, 업데이트, 요소 삭제 및 이를 반복 처리하는 방법을 보여줍니다.

## 심층 분석

연관 배열의 개념은 Perl과 PHP 같은 스크립트 언어에서의 사용으로 거슬러 올라가며, 이들 언어에서 데이터 컬렉션을 관리하는 유연성을 제공합니다. C#에서는 `.NET Framework 2.0`에서 도입된 `Dictionary<TKey, TValue>`가 사실상의 구현체로, 해시 테이블에 데이터를 저장하여 조회, 추가, 삭제를 효율적으로 수행합니다.

그러나 딕셔너리가 매우 다양하게 사용될 수 있음에도 불구하고 항상 최선의 선택이 되는 것은 아닙니다. 순서가 지정된 컬렉션을 유지해야 한다면, 삽입 및 삭제 작업이 느리다는 단점을 감수하면서도 정렬된 순서를 제공하는 `SortedDictionary<TKey, TValue>` 또는 `SortedList<TKey, TValue>`을 찾아볼 수 있습니다. 스레드 안정성이 요구되는 시나리오에서는 `ConcurrentDictionary<TKey, TValue>`가 오버헤드를 추가하지만, 수동 락을 사용하지 않고도 여러 스레드에서 안전하게 접근할 수 있도록 보장합니다.

결국, C#에서 연관 배열 구현체를 선택하는 것은 순서, 성능, 스레드 안정성에 대한 특정한 요구에 따라 달라집니다.
