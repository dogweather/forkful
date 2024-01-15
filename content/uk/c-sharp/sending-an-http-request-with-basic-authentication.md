---
title:                "Надсилання http запиту з основною аутентифікацією"
html_title:           "C#: Надсилання http запиту з основною аутентифікацією"
simple_title:         "Надсилання http запиту з основною аутентифікацією"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##Чому

Базова аутентифікація HTTP є однією з найбільш поширених технологій автентифікації в Інтернеті. Вона забезпечує простий і надійний спосіб перевірити право доступу до веб-ресурсів. Часто використовується для захисту конфіденційної інформації, такої як паролі та банківські дані.

##Як

```c#
using System;
using System.Net.Http;

class Program
{
	static async void SendHTTPRequest()
	{
		try 
		{
			using (var client = new HttpClient())
			{
				// Додати базову аутентифікацію до запиту
				var authValue = new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(System.Text.ASCIIEncoding.ASCII.GetBytes($"{username}:{password}")));
				client.DefaultRequestHeaders.Authorization = authValue;

				// Встановити URI для запиту
				var uri = new Uri("https://www.example.com/api");

				// Відправити запит GET з базовою автентифікацією
				var result = await client.GetAsync(uri);

				// Отримати тіло запиту як рядок
				var response = await result.Content.ReadAsStringAsync();

				// Вивести результат
				Console.WriteLine(response);
			}
		} 
		catch (Exception ex) 
		{
			Console.WriteLine(ex.Message);
		}
	}
}
```

При виконанні цього коду, ми встановлюємо базову аутентифікацію у заголовок запиту HTTP, використовуючи метод Convert.ToBase64String для кодування логіну та паролю з текстового формату у формат, який може бути передано в HTTP. Потім ми створюємо об'єкт частини HttpClient, додаємо до нього базову аутентифікацію та відправляємо запит GET до вказаного URL з використанням цього об'єкта. Результат отримується у вигляді рядка, який містить вихідний код веб-сторінки.

##Поглиблене дослідження

У технічних термінах, базова аутентифікація HTTP передає логін та пароль як базове значення авторизаційного заголовка. Зазвичай це використовується разом з HTTPS для захисту від перехоплення даних мережевими пристроями. Базова аутентифікація є стандартною для HTTP і може бути використана в більшості мов програмування, включаючи C#.

##Дивись також

- [Документація Microsoft про HttpClient](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Стаття про базову аутентифікацію HTTP на сайті MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Приклад використання базової аутентифікації на сайті GitHub](https://developer.github.com/v3/auth/#basic-authentication)