---
title:                "Fish Shell: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

# 왜 

CSV 파일은 데이터를 쉽게 저장하고 관리하기 위해 널리 사용되는 형식입니다. 만약 당신이 데이터를 다루거나 분석하는 일을 한다면, CSV 파일을 다루기 위한 프로그래밍 기술을 익히는 것은 아주 유용한 일이 될 것입니다. 이 글에서는 Fish Shell을 사용하여 CSV 파일을 다루는 법을 알려드리겠습니다.

## 어떻게 

우선, `csv` 라이브러리를 설치해야 합니다. Fish Shell에서는 `fisher`를 사용하여 간단하게 설치할 수 있습니다. 다음 명령어를 사용하여 설치해보세요.

```
fisher add patrickf1/fish-csv
```

이제 `csv` 라이브러리를 사용하여 CSV 파일을 다루는 예시를 살펴보겠습니다. 우리가 다음과 같은 `example.csv` 파일을 가지고 있다고 가정해봅시다.

```
name,age,location
John,27,New York
Anna,32,Seoul
David,23,London
```

이제 Fish Shell에서 다음과 같은 코드를 실행해보세요.

```
set -l data (csv reader example.csv)
for line in $data
    echo $line[name]
end
```

위 코드는 CSV 파일에서 `name` 열의 데이터를 읽어와 출력하는 간단한 예시입니다. 정말 쉽죠?

## 더 들어가기

Fish Shell에서 CSV 파일을 다루는 경우에 유용한 몇 가지 팁을 소개해드리겠습니다. 첫 번째는 `csv` 라이브러리의 `table` 명령어를 사용하는 것입니다. 이 명령어를 사용하면 CSV 파일의 데이터를 표 형태로 출력할 수 있습니다. 다음과 같이 사용해보세요.

```
set -l data (csv reader example.csv)
csv table $data
```

또 다른 팁은 `csv` 라이브러리의 `writer` 명령어를 사용하여 새로운 CSV 파일을 만드는 것입니다. 다음과 같은 코드를 실행해보세요.

```
set -l data (csv reader example.csv)
csv writer new_file.csv $data
```

마지막으로 CSV 파일을 다루면서 오류가 발생할 수도 있습니다. 이때는 `debug` 명령어를 사용하여 오류를 찾을 수 있습니다. `debug csv ...` 형식으로 사용하면 됩니다.

## 더 알아보기

Fish Shell을 사용하여 CSV 파일을 다루는 방법에 대해 간단히 살펴보았지만, 언제나 그렇듯이 더 많은 것을 알고 싶을 수 있습니다. 아래 링크들을 참고하여 더 깊이 있는 정보를 얻어보세요.

### 관련 링크들

- [Fish Shell 공식 페이지](https://fishshell.com/)
- [Fish Shell 시작하기](https://fishshell.com/docs/current/tutorial.html)
- [csv 라이브러리의 GitHub 페이지](https://github.com/patrickf1/fish-csv)
- [csv 라이브러리 문서](https://fishshell.com/docs/current/cmds/csv.html)
- [Fish Shell 관련 블로그](https://fishshell.com/docs/current/tutorial.html)
- [여러 가지 방식으로 CSV 파일 다루기](https://fishshell.com/docs/current/tutorial.html#handling-different-csv-formats)

## 더 알아보기