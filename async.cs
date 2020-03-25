static async Task<List<City>> AllCitiesToVisit(Speaker speaker)
{
   var cities = new ArrayList();
   talks = await speaker.GetTalks();
   foreach (talk in talks) {
     var conferences = await talk.GetConferences();
     foreach (conf in conferences) {
       var confCities = await conf.GetCities();
       foreach (city in cities) {
         cities.add(city);
       }
     }
   }
}
