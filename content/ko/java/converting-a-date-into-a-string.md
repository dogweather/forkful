---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 및 왜?
날짜를 문자열로 변환하는 것은 특정 날짜를 표준적으로 표현된 문자열 형태로 변환하는 프로그래밍 동작을 말합니다. 이는 특정 포맷의 문자열이 필요한 곳에서 날짜 데이터를 사용하거나 저장할 때 이용됩니다.

## 어떻게 할까요:
Java에서는 java.time과 java.text.SimpleDateFormat 클래스를 이용해 날짜를 문자열로 변환할 수 있습니다.

아래에 java.time의 `LocalDate`와 `DateTimeFormatter`를 사용하는 예시를 보여드리겠습니다:

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        String dateString = date.format(formatter);

        System.out.println(dateString);  // '2021-09-23'와 같은 형태로 출력됩니다.
    }
}
```

이 다음에는 java.text.SimpleDateFormat를 사용하는 예시를 보여드리겠습니다:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    public static void main(String[] args) {
        Date date = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
        String dateString = formatter.format(date);

        System.out.println(dateString);  // '2021-09-23'와 같은 형태로 출력됩니다.
    }
}
```

## 깊게 알아보기:
날짜를 문자열로 변환하는 기능은 프로그래밍 초기부터 존재했던 기능입니다. 숫자로 표현된 날짜를 사람이 이해하기 쉬운 문자열로 변환하는 기능이 필요했기 때문이죠. 자바에서는 java.time 패키지가 이러한 기능을 제공하며, 이전엔 java.util.Date와 java.text.SimpleDateFormat를 통해 동일한 역할을 수행했습니다.

이들 간의 주요 차이점은 java.time 패키지가 Java 8에서 새롭게 도입되어 더 직관적이고 유연성이 높다는 것입니다.

## 참고자료:
1. Java DateTimeFormatter 공식 문서: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
2. Java SimpleDateFormat 공식 문서: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
3. Java Date to String 변환에 대한 스택오버플로우 토론: https://stackoverflow.com/questions/5677472/java-date-to-string-conversion