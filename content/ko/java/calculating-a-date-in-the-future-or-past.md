---
title:                "Java: 미래나 과거의 날짜 계산하기"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래 또는 과거로 계산하는 일에 참여하는 이유는 무엇입니까? 이것은 많은 프로그램, 애플리케이션 및 웹 사이트에서 필요한 일입니다. 예를 들어, 생일 계산기, 약속 알리미, 이벤트 플래너 등이 모두 날짜를 처리하는 데 사용될 수 있습니다. 또한 날짜를 계산하는 것은 프로그램을 더 인간적으로 만들어주기 때문에 중요합니다.

## 어떻게

### Java에서의 날짜 계산

날짜를 계산하는 가장 쉬운 방법은 Java에서 제공하는 기본 클래스인 `Calendar`를 사용하는 것입니다. 다음은 미래의 날짜를 계산하는 예제 코드입니다.

```Java
// 현재 날짜 가져오기
Calendar cal = Calendar.getInstance();

// 1년 뒤로 이동
cal.add(Calendar.YEAR, 1);

// 결과 출력
System.out.println("1년 뒤 날짜: " + cal.getTime());
```

위의 코드를 실행하면 현재 날짜에서 1년 뒤로 이동한 날짜가 출력될 것입니다. 마찬가지로 과거의 날짜를 계산하려면 `cal.add(Calendar.YEAR, -1)`와 같이 음수 값을 전달하면 됩니다.

### 날짜 포맷 설정

`Calendar` 클래스의 `getTime()` 메소드는 `Date` 클래스의 인스턴스를 반환합니다. 해당 인스턴스를 사용하여 출력되는 날짜의 포맷을 설정할 수 있습니다. 다음은 날짜를 `yyyy년 MM월 dd일`의 형식으로 출력하는 예제 코드입니다.

```Java
// 현재 날짜 가져오기
Calendar cal = Calendar.getInstance();

// 날짜 포맷 설정
SimpleDateFormat sdf = new SimpleDateFormat("yyyy년 MM월 dd일");
        
// 결과 출력
System.out.println("포맷 설정된 날짜: " + sdf.format(cal.getTime()));
```

### 다른 날짜 단위 계산

`Calendar` 클래스는 `add()` 뿐만 아니라 다른 날짜 단위를 계산하는 다양한 메소드를 제공합니다. 다음은 월을 계산하는 예제 코드입니다.

```Java
// 현재 날짜 가져오기
Calendar cal = Calendar.getInstance();

// 3개월 뒤로 이동
cal.add(Calendar.MONTH, 3);

// 결과 출력
System.out.println("3개월 뒤 날짜: " + cal.getTime());
```

## 딥 다이브

Java에서 날짜를 계산하는 데 사용되는 `Calendar` 클래스는 Greogorian 달력을 기반으로 합니다. 따라서 그레고리오 달력에 따라 지원하지 않는 윤년도 존재할 수 있으며, 이는 오랜 기간의 날짜 계산에 영향을 줄 수 있습니다. 또한 해당 클래스는 시스템 날짜에 영향을 받기 때문에 결과가 잘못된 경우가 있을 수 있습니다. 따라서 복잡한 날짜 계산을 할 때는 외부 라이브러리를 사용하는 것이 좋습니다.

## 봐주십시오

- [Java Calendar Class](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html)
- [SimpleDateFormat Class](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Joda-Time Library](https://www.joda.org/joda-time/)