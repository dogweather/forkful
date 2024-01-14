---
title:    "Clojure: 미래 또는 과거 날짜 계산하기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 왜

계산하는 이유는 오늘보다 미래나 과거의 특정 날짜를 알기 위해서일 수 있습니다.

# 어떻게

우선, 새로운 날짜를 계산하는 가장 쉬운 방법은 현재 날짜를 기준으로 원하는 날짜의 일 수를 더하거나 빼는 것입니다. 예를 들어, 다음과 같이 작성할 수 있습니다:

```Clojure
(import 'java.util.Calendar)

(def cur-date (Calendar/getInstance))
(.add cur-date Calendar/DATE 10)
```
위의 코드는 현재 날짜에서 10일을 더한 날짜를 계산하므로, 다음과 같은 결과가 나타납니다:

```
#inst "2021-03-16T07:14:02.435-00:00"
```

마찬가지로, 과거 날짜를 계산할 수도 있습니다. 예를 들어, 아래의 코드는 현재 날짜에서 10일을 뺀 날짜를 계산합니다.

```Clojure
(import 'java.util.Calendar)

(def cur-date (Calendar/getInstance))
(.add cur-date Calendar/DATE -10)
```

위 코드의 결과는 다음과 같습니다:

```
#inst "2021-02-24T07:14:02.435-00:00"
```

# 깊게 들어가기

더 정확한 날짜 계산을 위해서는 조금 더 복잡한 방법을 사용할 수 있습니다. 예를 들어, `clojure.java-time` 라이브러리를 사용할 수 있습니다. 이 라이브러리는 Clojure에서 날짜와 시간을 다루는 데 유용한 함수들을 제공합니다. 다음은 `clojure.java-time` 라이브러리를 사용하여 현재 날짜에서 10일 뒤의 날짜를 계산하는 예제 코드입니다.

```Clojure
(require '[java-time :as jt])
(jt/plus (jt/today) (jt/days 10))
```

위 코드의 결과는 다음과 같습니다:

```
#object[java.time.LocalDate 0x1525513 2021-03-26]
```

더 많은 예제와 정보는 `clojure.java-time`의 공식 문서를 참고하시기 바랍니다.

# 참고

- [Official Clojure Documentation](https://clojure.org/)
- [Clojure for the Brave and True](https://www.braveclojure.com/)
- [Clojure for Data Science](https://gist.github.com/marsam/9397486)