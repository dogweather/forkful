---
title:                "연관 배열 사용하기"
date:                  2024-01-30T19:12:02.486329-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

Java에서, 연관 배열 또는 맵은 데이터 조회와 조작을 효율적으로 수행할 수 있도록 키-값 쌍을 저장할 수 있게 해줍니다. 프로그래머들은 항목의 발생 횟수를 세거나 사용자를 그들의 권한에 매핑하기 같은 작업을 위해 이들을 사용하는데, 이는 빠른 접근 및 업데이트를 제공하기 때문입니다.

## 방법:

Java는 몇몇 언어들처럼 내장 연관 배열을 제공하지 않지만, `Map` 인터페이스와 `HashMap`, `TreeMap`과 같은 클래스를 제공하여 그 역할을 수행합니다. 여기 `HashMap` 사용법이 있습니다:

```Java
import java.util.HashMap;
import java.util.Map;

public class LearnMaps {
    public static void main(String[] args) {
        // HashMap 생성
        Map<String, Integer> ageOfFriends = new HashMap<>();
        
        // 요소 추가
        ageOfFriends.put("Alice", 24);
        ageOfFriends.put("Bob", 30);
        ageOfFriends.put("Charlie", 28);

        // 요소 접근
        System.out.println("Alice의 나이: " + ageOfFriends.get("Alice"));
        
        // 존재하지 않는 키 처리
        System.out.println("맵에 없는 사람의 나이: " + ageOfFriends.getOrDefault("Dan", -1));

        // 요소 순회
        for (Map.Entry<String, Integer> entry : ageOfFriends.entrySet()) {
            System.out.println(entry.getKey() + "는 " + entry.getValue() + "살입니다.");
        }
    }
}
```

샘플 출력:

```
Alice의 나이: 24
맵에 없는 사람의 나이: -1
Alice는 24살입니다.
Bob은 30살입니다.
Charlie는 28살입니다.
```

`HashMap`은 하나의 구현일 뿐입니다. 키가 유일하며 정렬이 필요한 경우, `TreeMap`을 고려해 보십시오. 삽입 순서를 유지하는 맵이 필요하다면, `LinkedHashMap`이 답입니다.

## 심화 학습

Java의 맵은 JDK 1.2에서 소개된 컬렉션 프레임워크의 일부이지만, 엔트리 순회를 더 쉽게 만들어주는 Java 8의 `forEach` 메소드 도입을 포함하여, 여러 해에 걸쳐 상당한 개선을 거쳐왔습니다. 맵 구현(`HashMap`, `LinkedHashMap`, `TreeMap`) 선택은 정렬과 성능에 대한 특정 요구 사항에 따라 결정되어야 합니다. 예를 들어, `HashMap`은 해시 함수가 요소를 버킷 사이에 적절히 분산시킨다고 가정할 때 기본 연산(get 및 put)에 대해 O(1) 시간 성능을 제공합니다. 그러나, 자연 순서 또는 사용자 정의 비교자를 기반으로 정렬이 필요한 경우, 삽입 및 조회에 O(log n) 시간이 걸리는 `TreeMap`이 가장 적합합니다.

`Map`이 도입되기 전에는 보통 키와 값에 대한 두 개의 평행 배열이나 효율성이 떨어지는 사용자 정의 데이터 구조를 사용하여 연관 배열을 구현했습니다. `Map`과 그 구현에 대한 현재 대안으로는 값을 효율적으로 찾기 위해 키가 필요한 경우에 사용할 수 있는 양방향 맵(BiMap in Google's Guava library)과 같은 전문 맵을 제공하는 타사 라이브러리가 포함될 수 있습니다. 그러나 Java의 대부분의 사용 사례에서는 표준 라이브러리의 맵이 태스크를 처리하기에 충분히 강력하고 유연합니다.
