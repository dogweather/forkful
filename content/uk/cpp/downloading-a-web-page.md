---
title:                "C++: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

В сучасному світі, коли доступ до інформації є надзвичайно важливим, іноді нам потрібно мати локальну копію веб-сторінки для подальшого використання. Наприклад, може знадобитися зберегти новини чи статтю для подальшого читання в автономному режимі. У цьому випадку завантаження веб-сторінки є потрібною задачею.

## Як

Для завантаження веб-сторінки у нашому коді ми будемо використовувати бібліотеку `cURL`, яка дозволяє виконувати HTTP запити. Нижче наведений приклад коду на C++, який демонструє як завантажити веб-сторінку та зберегти її у вигляді текстового файлу:

```C++
#include <iostream>
#include <curl/curl.h>

using namespace std;

// Callback-функція для відстеження прогресу завантаження
static int progress_callback(void* clientp, double dltotal, double dlnow, double ultotal, double ulnow) {
    // Перевіряємо чи не було помилки під час завантаження
    if (dltotal == 0) { 
        cout << "Помилка завантаження веб-сторінки!" << endl;
        return 1; 
    }

    // Обчислюємо прогрес завантаження у відсотках
    double progress = dlnow / dltotal * 100;
    cout << "Завантажено " << progress << "%\r" << flush;
    return 0;
}

int main() {
    // Ініціалізуємо бібліотеку cURL
    curl_global_init(CURL_GLOBAL_ALL);

    // Вибираємо URL адресу веб-сторінки
    const char* url = "https://example.com";

    // Ініціалізуємо змінні для збереження даних про завантажену веб-сторінку
    string result;
    CURL* curl = curl_easy_init();
    CURLcode res;
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &result);
    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0);
    curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, &progress_callback);

    // Виконуємо запит на завантаження веб-сторінки
    res = curl_easy_perform(curl);

    // Перевіряємо результат виконання запиту
    if (res != CURLE_OK) {
        cout << "Помилка завантаження веб-сторінки: " << endl;
        cout << curl_easy_strerror(res) << endl;
    }
    else {
        // Зберігаємо результат у текстовий файл
        FILE* fp = fopen("webpage.txt", "w");
        fputs(result.c_str(), fp);
        fclose(fp);
        cout << "Веб-сторінка успішно завантажена та збережена у файлі webpage.txt" << endl;
    }

    // Закриваємо бібліотеку cURL
    curl_easy_cleanup(curl);
    curl_global_cleanup();

    return 0;
}
```

В результаті виконання цього коду у вас з'явиться файл `