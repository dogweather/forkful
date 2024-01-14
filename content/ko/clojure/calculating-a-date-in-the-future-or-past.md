---
title:                "Clojure: 미래 혹은 과거의 날짜 계산하기"
simple_title:         "미래 혹은 과거의 날짜 계산하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

날짜를 미래나 과거로 계산하는 일에 참여해야 할 이유는 다양합니다. 예를 들어, 스마트폰 어플리케이션에서 약속 날짜를 설정하거나 생일을 계산할 때가 있습니다. 또는 수학적인 문제를 해결하는 과정에서도 날짜 계산이 필요할 수 있습니다. 어떤 경우에도 쉽고 간단하게 날짜를 계산하는 방법을 알아보겠습니다.

# 어떻게

```Clojure
(defn calculate-date [num-of-days]
  (.getTime 
    (java.util.Date. 
      (+ 
        (.getTime (java.util.Date. today) (days num-of-days)))))
```

위의 예제 코드는 현재 날짜로부터 인자로 전달된 일수를 더한 날짜를 계산하는 함수입니다. 함수 내부에서는 `java.util.Date` 클래스의 `.getTime` 메소드를 사용하여 현재 날짜를 얻은 뒤, `days` 함수를 사용하여 일수를 더한 다음 `java.util.Date` 클래스의 인스턴스를 생성합니다. 마지막으로 `.getTime` 메소드를 다시 사용하여 더한 날짜를 리턴합니다.

이제 함수를 호출하여 날짜를 계산해보겠습니다.

```Clojure
(def today (calculate-date 10))

;; 결과
#inst "2020-06-12T07:22:09.468-00:00"
```

10일이 더해진 날짜를 얻을 수 있었습니다. 비슷한 방식으로 어떤 날짜를 기준으로 미래나 과거의 날짜를 계산할 수 있습니다.

# 딥 다이브

실제로 날짜 계산에는 더 많은 처리 과정과 요구사항이 있을 수 있습니다. 예를 들어, 윤년을 처리하거나 특정 월의 마지막 날을 정확하게 계산해야 할 수도 있습니다. 따라서 실제 프로젝트에서 날짜 계산을 할 때에는 이러한 요구사항을 고려하여 코드를 작성해야 합니다.

또한 날짜를 계산하는 다양한 라이브러리나 함수들이 존재하며, 각각의 장단점이 있을 수 있습니다. 따라서 다양한 방식으로 날짜를 계산하는 방법을 알고 있으면 유용합니다.

# 비슷한 주제

- [Clojure 스트링 다루기](https://clojure.org/guides/learn/common_data_structures#_strings)
- [Clojure 컬렉션 다루기](https://clojure.org/guides/learn/common_data_structures#_collection_manipulation)
- [Clojure 날짜 다루기](https://clojure.github.io/java.time-api/#clojure.java-time)

# 참고자료

- [Clojure 공식 문서](https://clojure.org/)
- [Clojure 한국 사용자 그룹](https://www.facebook.com/groups/ClojureKr/)