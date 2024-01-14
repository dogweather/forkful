---
title:    "Bash: 날짜를 문자열로 변환하기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 일은 프로그래밍에서 자주 발생하는 일 중 하나입니다. 예를 들어, 사용자 인터페이스에서 날짜를 표시하기 위해 날짜를 문자열로 변환해야 할 때가 있습니다. 이 작업을 위해서는 Bash에서 기본적으로 제공하는 다른 도구들을 사용할 수 있으며, 이를 통해 더 정확하고 유연한 작업을 수행할 수 있습니다.

## 어떻게

```Bash
# 현재 날짜 변수에 할당하기
date=$(date +%Y-%m-%d)

# 날짜를 문자열로 변환하기
string_date=$(date -d "$date" +"%Y/%m/%d")

# 변환된 문자열 출력하기
echo "오늘의 날짜는 $string_date 입니다."
```

Output:

```Bash
오늘의 날짜는 2020/07/13 입니다.
```

위의 예시에서는 `date` 명령어를 사용하여 현재 날짜를 가져오고, `date -d`를 사용하여 날짜를 원하는 형식으로 변환할 수 있습니다. 이렇게 하면 변수에 할당된 날짜를 원하는 형태로 동적으로 변환할 수 있습니다.

더 복잡한 방법으로는 `date -u`를 사용하여 UTC 시간을 나타내고, `--iso-8601` 옵션을 사용하여 ISO 8601 규격에 따라 날짜를 표시할 수도 있습니다.

## 딥 다이브

날짜를 문자열로 변환하는 과정에서 많은 옵션을 사용할 수 있습니다. 예를 들어, 사용자가 원하는 형태에 맞게 날짜와 시간을 조합하여 출력할 수 있습니다. 또한 `date` 명령어에는 다양한 옵션을 제공하는데, 이 중 일부는 날짜를 문자열로 변환하는 데 유용할 수 있습니다.

또한 문자열로 변환된 날짜는 다른 변수에 할당하거나 파일에 저장하고, 나중에 사용할 수도 있습니다. 이를 통해 날짜를 다시 사용하거나 추적할 수 있습니다.

## 참고

- [Bash에서 날짜 형식 변경하기](https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/)
- [GNU Bash에서 date명령어 사용하기](https://www.gnu.org/software/bash/manual/html_node/Date-Input-Formats.html)