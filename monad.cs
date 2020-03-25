static Task<List<City>> AllCitiesToVisit(Speaker speaker)
{
    return
        speaker
        .GetTalks()
        .TraverseM(talk => talk.GetConferences())
        .TraverseM(conf => conf.GetCities());
}
